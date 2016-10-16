uniform vec2 trg_size;

in vec3 position;
in vec4 color;

out vec4 frag_color;

void main() {

  gl_Position.x = (position.x * 2.0) / trg_size.x - 1.0;
  gl_Position.y = 1.0 - (position.y * 2.0) / trg_size.y;
  gl_Position.z = 0.;
  gl_Position.w = 1.0;

  frag_color = color;

}
