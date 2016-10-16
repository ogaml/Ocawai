uniform sampler2D src_tex;
uniform sampler2D blurred_tex;

in vec2 frag_uv;

out vec4 frag_color;

vec4 AdjustSaturation(in vec4 color, in float saturation)
{
	float grey = dot(color, vec4(vec3(0.3, 0.59, 0.11), 0.0));
	vec4 grey_color = vec4(grey, grey, grey, 0.0);
	
	return mix(grey_color, color, saturation);
}

void main() 
{
  vec4 bloom = AdjustSaturation(texture(blurred_tex, frag_uv), 1.5);
  vec4 base  = AdjustSaturation(texture(src_tex, frag_uv), 0.3);
  frag_color = base * 2. + bloom * 2.;
}
