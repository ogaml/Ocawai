uniform vec2 win_size;
uniform vec2 tex_size;
uniform sampler2D mtexture;

in vec3 position;
in vec2 uv;
in vec4 color;

out vec4 frag_color;
out vec2 frag_uv;

void main() {

  gl_Position.x = (2.0 * position.x) / win_size.x;
  gl_Position.y = - (2.0 * position.y) / win_size.y;
  gl_Position.z = 0.0;
  gl_Position.w = 1.0;

  frag_color = color;
  frag_uv.x = uv.x / tex_size.x;
  frag_uv.y = 1.0 - uv.y / tex_size.y;

}
