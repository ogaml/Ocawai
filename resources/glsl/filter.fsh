
uniform sampler2D src_tex;

in vec2 frag_uv;

out vec4 frag_color;

const float threshold = 0.15;
const float multiplier = 5.0;

void main()
{
  vec4 sourceFragment = texture(src_tex, frag_uv);
  float luminance = sourceFragment.r * 0.5 + sourceFragment.g * 0.6 + sourceFragment.b * 0.5;
  sourceFragment *= clamp(luminance - threshold, 0.0, 1.0) * multiplier;
  frag_color = sourceFragment;
}
