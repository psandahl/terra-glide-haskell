/*
 * Water vertex shader.
 */
#version 330 core

// Position attribute.
layout (location = 0) in vec3 position;

// Texture coordinate attribute.
layout (location = 1) in vec2 texCoord;

// The complete transformation matrix.
uniform mat4 mvpMatrix;

// Clip space coordinate to be transfered to the fragment shader.
out vec4 clipSpace;

// Texture coordinates for the DuDv map.
out vec2 dudvCoord;

void main()
{
  // Make texture coordinates.
  dudvCoord = texCoord;

  // Transform the position to clip space.
  clipSpace = mvpMatrix * vec4(position, 1);
  gl_Position = clipSpace;
}
