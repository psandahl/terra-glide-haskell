/*
 * Water fragment shader.
 */
#version 330 core

in vec4 clipSpace;

uniform vec3 waterColor;
uniform sampler2D refractionTexture;

out vec4 color;

void main()
{
  vec2 ndc = (clipSpace.xyz / clipSpace.w).xy / 2 + 0.5;
  color = vec4(mix(texture2D(refractionTexture, ndc).rgb, waterColor, 0.35), 1);
}
