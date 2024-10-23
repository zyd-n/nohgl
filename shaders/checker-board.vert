#version 330 core

layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 uv;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec2 UV;
out vec3 FragPos;
out vec3 Normal;

void main()
{
  FragPos = vec3(model * vec4(position, 1.0));
  Normal = normal;
  UV = vec2(uv.x, uv.y);
  gl_Position = projection * view * model * vec4(position, 1.0);
}
