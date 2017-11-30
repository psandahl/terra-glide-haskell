/*
 * Water vertex shader.
 */
#version 330 core

// Position attribute.
layout (location = 0) in vec3 position;

// The complete transformation matrix.
uniform mat4 mvpMatrix;

// Clip space coordinate to be transfered to the fragment shader.
out vec4 clipSpace;

void main()
{
  // Transform the position to clip space.
  clipSpace = mvpMatrix * vec4(position, 1);
  gl_Position = clipSpace;
}
