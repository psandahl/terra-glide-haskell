/*
 * Fragment shader for terrain tiles.
 */
#version 330 core

// The color calculated by the vertex shader.
in vec3 fragmentColor;

// The final color value.
out vec4 color;

void main()
{
  color = vec4(fragmentColor, 1);
}
