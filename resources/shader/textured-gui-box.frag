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

void main()
{
  color = vec4(texture2D(texture, vTexCoord).rgb, 1);
}
