# Triangular DQ coefficients and high-order mesh

#### 介绍
本项目给出了一个web程序。这个程序可以(1)帮助用户生成高阶三角形网格，(2)计算生成三角形微分求积法权系数所需要的关键系数矩阵。

#### 软件架构
采用go语言编写后端程序。html+JavaScript作为后端。与科学计算有关的功能定义在mathematica的脚本文件。


#### 安装教程

1.  必须安装Mathematica 13.0及以上版本
2.  将Mathematica的math.exe所在路径添加到系统的环境变量path内。

#### 使用说明

1.  配置完后之后，用命令行运行main.exe
2.  访问http://localhost:18085
3.  在DQM计算标签中，计算得到的c10和c01分别表示关于ξ和η一阶偏导的系数矩阵；cL表示考虑积分点的修正矩阵。