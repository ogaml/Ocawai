uniform sampler2D mtexture;

in vec4 frag_color;
in vec2 frag_uv;

out vec4 out_color;

void main() {

  out_color = texture(mtexture, frag_uv) * frag_color;
  
}
