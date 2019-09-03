/*
 * This file is part of FFmpeg.
 *
 * FFmpeg is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * FFmpeg is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with FFmpeg; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA
 */

#include <string.h>

#include "libavutil/avassert.h"
#include "libavutil/mem.h"
#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"

#include "avfilter.h"
#include "formats.h"
#include "internal.h"
#include "scale.h"
#include "video.h"
#include "vaapi_vpp.h"

typedef struct ScaleVAAPIContext {
    VAAPIVPPContext vpp_ctx; // must be the first field

    char *output_format_string;

    char *w_expr;      // width expression string
    char *h_expr;      // height expression string

    int keep_ar;

    int64_t num_boxes;
    int *xs, *ys, *ws, *hs;
    char *xs_str, *ys_str;
    char *ws_str, *hs_str;
    AVFrame *black_frame;

} ScaleVAAPIContext;

static int scale_vaapi_config_output(AVFilterLink *outlink)
{
    AVFilterLink *inlink     = outlink->src->inputs[0];
    AVFilterContext *avctx   = outlink->src;
    VAAPIVPPContext *vpp_ctx = avctx->priv;
    ScaleVAAPIContext *ctx   = avctx->priv;
    int err;

    if ((err = ff_scale_eval_dimensions(ctx,
                                        ctx->w_expr, ctx->h_expr,
                                        inlink, outlink,
                                        &vpp_ctx->output_width, &vpp_ctx->output_height)) < 0)
        return err;

    err = ff_vaapi_vpp_config_output(outlink);
    if (err < 0)
        return err;

    if (inlink->sample_aspect_ratio.num)
        outlink->sample_aspect_ratio = av_mul_q((AVRational){outlink->h * inlink->w, outlink->w * inlink->h}, inlink->sample_aspect_ratio);
    else
        outlink->sample_aspect_ratio = inlink->sample_aspect_ratio;

    return 0;
}

static void biggest_box(int num_boxes, int* ws, int* hs, int* width, int* height)
{
    int largestWidth = 0;
    int largestHeight = 0;

    for(int i = 0; i < num_boxes; ++i)
    {
        if(ws[i] > largestWidth)
            largestWidth = ws[i];
        if(hs[i] > largestHeight)
            largestHeight = hs[i];
    }

    *width = largestWidth;
    *height = largestHeight;
}

static int scale_vaapi_filter_frame(AVFilterLink *inlink, AVFrame *input_frame)
{
    AVFilterContext *avctx   = inlink->dst;
    AVFilterLink *outlink    = avctx->outputs[0];
    VAAPIVPPContext *vpp_ctx = avctx->priv;
    ScaleVAAPIContext *ctx   = avctx->priv;
    AVFrame *output_frame    = NULL;
    VASurfaceID input_surface, output_surface;
    VAProcPipelineParameterBuffer params;
    VARectangle input_region;
    VARectangle output_region;
    int err;

    av_log(avctx, AV_LOG_DEBUG, "Filter input: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(input_frame->format),
           input_frame->width, input_frame->height, input_frame->pts);

    if (vpp_ctx->va_context == VA_INVALID_ID)
        return AVERROR(EINVAL);

    if(ctx->black_frame == NULL && (ctx->num_boxes > 0))
    {
        int width, height;
        biggest_box(ctx->num_boxes, &ctx->ws[0], &ctx->hs[0], &width, &height);

        ctx->black_frame = ff_get_video_buffer(outlink, width, height);
        if (!ctx->black_frame) {
            err = AVERROR(ENOMEM);
            goto fail;
        }
    }

    input_surface = (VASurfaceID)(uintptr_t)ctx->black_frame->data[3];

    for(int i = 0; i < ctx->num_boxes; ++i)
    {
        memset(&params, 0, sizeof(params));

        input_region = (VARectangle) {
            .x      = 0,
            .y      = 0,
            .width  = ctx->ws[i],
            .height = ctx->hs[i]
        };

        params.surface = input_surface;
        params.surface_region = &input_region;
        params.surface_color_standard = ff_vaapi_vpp_colour_standard(input_frame->colorspace);

        output_region = (VARectangle) {
            .x = ctx->xs[i],
            .y = ctx->ys[i],
            .width = ctx->ws[i],
            .height = ctx->hs[i]
        };

        params.output_region = &output_region;

        params.output_background_color = 0xff00ff00;
        params.output_color_standard = params.surface_color_standard;
        params.pipeline_flags = 0;
        params.filter_flags = VA_FILTER_SCALING_HQ;

        err = ff_vaapi_vpp_render_picture(avctx, &params, (VASurfaceID)(uintptr_t)input_frame->data[3]);
        if (err < 0)
            goto fail;
    }

    input_surface = (VASurfaceID)(uintptr_t)input_frame->data[3];
    av_log(avctx, AV_LOG_DEBUG, "Using surface %#x for scale input.\n",
           input_surface);

    output_frame = ff_get_video_buffer(outlink, vpp_ctx->output_width,
                                       vpp_ctx->output_height);
    if (!output_frame) {
        err = AVERROR(ENOMEM);
        goto fail;
    }

    output_surface = (VASurfaceID)(uintptr_t)output_frame->data[3];
    av_log(avctx, AV_LOG_DEBUG, "Using surface %#x for scale output.\n",
           output_surface);

    memset(&params, 0, sizeof(params));

    input_region = (VARectangle) {
        .x      = input_frame->crop_left,
        .y      = input_frame->crop_top,
        .width  = input_frame->width -
                 (input_frame->crop_left + input_frame->crop_right),
        .height = input_frame->height -
                 (input_frame->crop_top + input_frame->crop_bottom),
    };

    params.surface = input_surface;
    params.surface_region = &input_region;
    params.surface_color_standard =
        ff_vaapi_vpp_colour_standard(input_frame->colorspace);

    // if the aspect ratios of the input region and the output region differ significantly...
    if (ctx->keep_ar && fabsf((float)input_region.width / input_region.height -
                              (float)vpp_ctx->output_width / vpp_ctx->output_height) > 0.01) {

        
        int orx = 0, ory = 0, orw = vpp_ctx->output_width, orh = vpp_ctx->output_height;
        if (input_region.width * vpp_ctx->output_height > vpp_ctx->output_width * input_region.height) {
            // Add vertical margins.
            orh = vpp_ctx->output_width * input_region.height / input_region.width;
            ory = (vpp_ctx->output_height - orh) / 2;
        } else {
            // Add horizontal margins.
            orw = vpp_ctx->output_height * input_region.width / input_region.height;
            orx = (vpp_ctx->output_width - orw) / 2;
        }

        output_region.x = orx;
        output_region.y = ory;
        output_region.width = orw;
        output_region.height = orh;
        params.output_region = &output_region;

    } else {
        params.output_region = NULL;
    }
    params.output_background_color = 0xff00ff00;
    params.output_color_standard = params.surface_color_standard;

    params.pipeline_flags = 0;
    params.filter_flags = VA_FILTER_SCALING_HQ;

    err = ff_vaapi_vpp_render_picture(avctx, &params, output_surface);
    if (err < 0)
        goto fail;

    err = av_frame_copy_props(output_frame, input_frame);
    output_frame->crop_left = 0;
    output_frame->crop_top = 0;
    output_frame->crop_right = 0;
    output_frame->crop_bottom = 0;
    if (err < 0)
        goto fail;

    av_frame_free(&input_frame);

    av_log(avctx, AV_LOG_DEBUG, "Filter output: %s, %ux%u (%"PRId64").\n",
           av_get_pix_fmt_name(output_frame->format),
           output_frame->width, output_frame->height, output_frame->pts);

    return ff_filter_frame(outlink, output_frame);

fail:
    av_frame_free(&input_frame);
    av_frame_free(&output_frame);
    return err;
}

static void parse_numbers(char* s, int* i)
{
    char* rd = s;
    char* token = NULL;
    int index = 0;

    while ((token = strtok_r(rd, "|", &rd)))
    {
        i[index] = atoi(token);
        ++index;
    }
}

static av_cold int scale_vaapi_init(AVFilterContext *avctx)
{
    VAAPIVPPContext *vpp_ctx = avctx->priv;
    ScaleVAAPIContext *ctx   = avctx->priv;

    ff_vaapi_vpp_ctx_init(avctx);
    vpp_ctx->pipeline_uninit = ff_vaapi_vpp_pipeline_uninit;

    if (ctx->output_format_string) {
        vpp_ctx->output_format = av_get_pix_fmt(ctx->output_format_string);
        if (vpp_ctx->output_format == AV_PIX_FMT_NONE) {
            av_log(avctx, AV_LOG_ERROR, "Invalid output format.\n");
            return AVERROR(EINVAL);
        }
    } else {
        // Use the input format once that is configured.
        vpp_ctx->output_format = AV_PIX_FMT_NONE;
    }

    ctx->xs = (int*)av_malloc_array(ctx->num_boxes, sizeof(int));
    parse_numbers(ctx->xs_str, ctx->xs);

    ctx->ys = (int*)av_malloc_array(ctx->num_boxes, sizeof(int));
    parse_numbers(ctx->ys_str, ctx->ys);

    ctx->ws = (int*)av_malloc_array(ctx->num_boxes, sizeof(int));
    parse_numbers(ctx->ws_str, ctx->ws);

    ctx->hs = (int*)av_malloc_array(ctx->num_boxes, sizeof(int));
    parse_numbers(ctx->hs_str, ctx->hs);

    ctx->black_frame = NULL;

    return 0;
}

static av_cold void scale_vaapi_uninit(AVFilterContext *avctx)
{
    ScaleVAAPIContext *ctx = avctx->priv;

    av_free(ctx->xs);
    av_free(ctx->ys);
    av_free(ctx->ws);
    av_free(ctx->hs);

    av_frame_free(&ctx->black_frame);

    ff_vaapi_vpp_ctx_uninit(avctx);
}

#define OFFSET(x) offsetof(ScaleVAAPIContext, x)
#define FLAGS (AV_OPT_FLAG_FILTERING_PARAM|AV_OPT_FLAG_VIDEO_PARAM)
static const AVOption scale_vaapi_options[] = {
    { "w", "Output video width",
      OFFSET(w_expr), AV_OPT_TYPE_STRING, {.str = "iw"}, .flags = FLAGS },
    { "h", "Output video height",
      OFFSET(h_expr), AV_OPT_TYPE_STRING, {.str = "ih"}, .flags = FLAGS },
    { "keep_ar", "Keep aspect ratio by padding",
      OFFSET(keep_ar), AV_OPT_TYPE_INT, { .i64 = 0 }, 0, INT_MAX, .flags = FLAGS },
    { "format", "Output video format (software format of hardware frames)",
      OFFSET(output_format_string), AV_OPT_TYPE_STRING, .flags = FLAGS },
    { "num_boxes", "The number of boxes to draw.",
      OFFSET(num_boxes), AV_OPT_TYPE_INT64, {.i64=0}, CHAR_MIN, 1024, .flags = FLAGS },
    { "xs", "array of xs",
      OFFSET(xs_str), AV_OPT_TYPE_STRING, { .str="0" }, CHAR_MIN, CHAR_MAX, .flags = FLAGS },
    { "ys", "array of ys",
      OFFSET(ys_str), AV_OPT_TYPE_STRING, { .str="0" }, CHAR_MIN, CHAR_MAX, .flags = FLAGS },
    { "ws", "array of widths",
      OFFSET(ws_str), AV_OPT_TYPE_STRING, { .str="0" }, CHAR_MIN, CHAR_MAX, .flags = FLAGS },
    { "hs", "array of heigts",
      OFFSET(hs_str), AV_OPT_TYPE_STRING, { .str="0" }, CHAR_MIN, CHAR_MAX, .flags = FLAGS },

    { NULL },
};

AVFILTER_DEFINE_CLASS(scale_vaapi);

static const AVFilterPad scale_vaapi_inputs[] = {
    {
        .name         = "default",
        .type         = AVMEDIA_TYPE_VIDEO,
        .filter_frame = &scale_vaapi_filter_frame,
        .config_props = &ff_vaapi_vpp_config_input,
    },
    { NULL }
};

static const AVFilterPad scale_vaapi_outputs[] = {
    {
        .name = "default",
        .type = AVMEDIA_TYPE_VIDEO,
        .config_props = &scale_vaapi_config_output,
    },
    { NULL }
};

AVFilter ff_vf_scale_vaapi = {
    .name          = "scale_vaapi",
    .description   = NULL_IF_CONFIG_SMALL("Scale to/from VAAPI surfaces."),
    .priv_size     = sizeof(ScaleVAAPIContext),
    .init          = &scale_vaapi_init,
    .uninit        = &scale_vaapi_uninit,
    .query_formats = &ff_vaapi_vpp_query_formats,
    .inputs        = scale_vaapi_inputs,
    .outputs       = scale_vaapi_outputs,
    .priv_class    = &scale_vaapi_class,
    .flags_internal = FF_FILTER_FLAG_HWFRAME_AWARE,
};
