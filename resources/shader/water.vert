/*
 * Water vertex shader.
 */
#version 330 core

layout (location = 0) in vec3 position;

uniform mat4 mvpMatrix;

out vec4 clipSpace;

void main()
{
  clipSpace = mvpMatrix * vec4(position, 1);
  gl_Position = clipSpace;
}
