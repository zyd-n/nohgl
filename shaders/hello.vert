#version 330 core

layout (location = 0) in vec3 Position;

uniform mat4 gScale;

void main()
{
  gl_Position = gScale * vec4(Position, 1.0);
}
