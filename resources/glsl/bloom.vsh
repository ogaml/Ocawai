uniform vec2 trg_size;

in vec3 position;
in vec2 uv;

out vec2 frag_uv;

void main()
{
  gl_Position.x = position.x;
  gl_Position.y = position.y;
  gl_Position.z = 0.0;
  gl_Position.w = 1.0;

  frag_uv = uv;
}


