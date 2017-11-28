/*
 * Fragment shader for the textured gui box.
 */
#version 330 core

// The texture used on the box.
uniform sampler2D texture;

// Interpolated texture coordinates.
in vec2 vTexCoord;

// The fragment color.
out vec4 color;

const float frame = 0.005;

void main()
{
  bool inside = vTexCoord.s > frame && vTexCoord.s < 1.0 - frame &&
                vTexCoord.t > frame && vTexCoord.t < 1.0 - frame;
  vec3 rgb = inside ? texture2D(texture, vTexCoord).rgb : vec3(1);

  color = vec4(rgb, 1);
}
