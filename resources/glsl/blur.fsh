uniform sampler2D src_tex;
uniform float radius;
uniform vec2 dir;
uniform vec2 trg_size;

in vec2 frag_uv;

out vec4 frag_color;

void main()
{
  vec2 ps;
       ps.x = radius / trg_size.x;
       ps.y = radius / trg_size.y;

  vec4 color = vec4(0.0);

  color += texture(src_tex, vec2(frag_uv.x - 4.0 * dir.x * ps.x, frag_uv.y - 4.0 * dir.y * ps.y)) * 0.0162162162;
  color += texture(src_tex, vec2(frag_uv.x - 3.0 * dir.x * ps.x, frag_uv.y - 3.0 * dir.y * ps.y)) * 0.0540540541;
  color += texture(src_tex, vec2(frag_uv.x - 2.0 * dir.x * ps.x, frag_uv.y - 2.0 * dir.y * ps.y)) * 0.1216216216;
  color += texture(src_tex, vec2(frag_uv.x - 1.0 * dir.x * ps.x, frag_uv.y - 1.0 * dir.y * ps.y)) * 0.1945945946;
  color += texture(src_tex, frag_uv) * 0.2270270270;
  color += texture(src_tex, vec2(frag_uv.x + 1.0 * dir.x * ps.x, frag_uv.y + 1.0 * dir.y * ps.y)) * 0.1945945946;
  color += texture(src_tex, vec2(frag_uv.x + 2.0 * dir.x * ps.x, frag_uv.y + 2.0 * dir.y * ps.y)) * 0.1216216216;
  color += texture(src_tex, vec2(frag_uv.x + 3.0 * dir.x * ps.x, frag_uv.y + 3.0 * dir.y * ps.y)) * 0.0540540541;
  color += texture(src_tex, vec2(frag_uv.x + 4.0 * dir.x * ps.x, frag_uv.y + 4.0 * dir.y * ps.y)) * 0.0162162162;

  frag_color = color; 

}
