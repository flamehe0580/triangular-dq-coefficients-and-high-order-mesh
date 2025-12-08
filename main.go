package main

import (
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
)

// 配置文件结构
type Config struct {
	Username string `json:"username"`
	Password string `json:"password"`
}

// 响应结构
type Response struct {
	ExcelData string `json:"excelData"`
	InputSvg  string `json:"inputSvg"`
	OutputSvg string `json:"outputSvg"`
}

// DQM响应结构
type DQMResponse struct {
	File1 string `json:"file1"` // c10_c01文件
	File2 string `json:"file2"` // cL文件
}

var appConfig Config

func main() {
	// 加载配置文件
	loadConfig("config.json")

	http.HandleFunc("/", serveHomePage)
	http.HandleFunc("/process", processHandler)
	http.HandleFunc("/dqm", dqmHandler)
	http.HandleFunc("/triangle_mesh_sample.xlsx", serveSampleFile)

	// 设置端口号为18085
	port := ":18085"
	fmt.Printf("服务器启动在 http://localhost%s\n", port)
	log.Fatal(http.ListenAndServe(port, nil))
}

// 加载配置文件
func loadConfig(filename string) {
	file, err := os.Open(filename)
	if err != nil {
		log.Fatalf("无法打开配置文件: %v", err)
	}
	defer file.Close()

	decoder := json.NewDecoder(file)
	err = decoder.Decode(&appConfig)
	if err != nil {
		log.Fatalf("无法解析配置文件: %v", err)
	}

	log.Printf("配置文件加载成功，用户名: %s", appConfig.Username)
}

// 验证用户凭据
func authenticate(username, password string) bool {
	return username == appConfig.Username && password == appConfig.Password
}

func serveHomePage(w http.ResponseWriter, r *http.Request) {
	// 设置CORS头，允许跨域请求
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, OPTIONS")

	// 提供HTML页面
	http.ServeFile(w, r, "index.html")
}

func serveSampleFile(w http.ResponseWriter, r *http.Request) {
	// 设置CORS头，允许跨域请求
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "GET, OPTIONS")

	// 设置Content-Disposition头，强制下载而不是在浏览器中打开
	w.Header().Set("Content-Disposition", "attachment; filename=\"triangle_mesh_sample.xlsx\"")
	w.Header().Set("Content-Type", "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")

	// 提供示例文件下载
	http.ServeFile(w, r, "triangle_mesh_sample.xlsx")
}

// DQM处理函数
func dqmHandler(w http.ResponseWriter, r *http.Request) {
	// 设置CORS头，允许跨域请求
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Content-Type", "application/json")

	// 处理预检请求
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}

	if r.Method != http.MethodPost {
		http.Error(w, "方法不允许", http.StatusMethodNotAllowed)
		return
	}

	// 解析表单数据
	err := r.ParseForm()
	if err != nil {
		http.Error(w, "无法解析表单", http.StatusBadRequest)
		return
	}

	// 验证用户凭据
	username := r.FormValue("username")
	password := r.FormValue("password")
	if !authenticate(username, password) {
		http.Error(w, "用户名或密码错误", http.StatusUnauthorized)
		return
	}

	// 获取参数
	mStr := r.FormValue("m")
	precisionStr := r.FormValue("precision")

	m, err := strconv.Atoi(mStr)
	if err != nil || m < 2 || m > 10 {
		http.Error(w, "参数m必须是2-10之间的整数", http.StatusBadRequest)
		return
	}

	precision, err := strconv.Atoi(precisionStr)
	if err != nil || precision < 1 || precision > 20 {
		http.Error(w, "代数精度必须是1-20之间的整数", http.StatusBadRequest)
		return
	}

	// 创建临时目录
	tempDir, err := os.MkdirTemp("", "dqm_processing")
	if err != nil {
		http.Error(w, "无法创建临时目录", http.StatusInternalServerError)
		return
	}
	defer os.RemoveAll(tempDir)

	// 调用Mathematica脚本
	cmd := exec.Command("math", "-script", "DQM2D.m", strconv.Itoa(m), strconv.Itoa(precision))
	cmd.Dir = "." // 需要设置为DQM2D.m所在目录

	output, err := cmd.CombinedOutput()
	if err != nil {
		log.Printf("Mathematica DQM执行错误: %s\n输出: %s", err, output)
		http.Error(w, "DQM计算失败", http.StatusInternalServerError)
		return
	}

	// 检查输出文件是否存在
	filename1 := fmt.Sprintf("c10_c01_order=%d.xlsx", m)
	filename2 := fmt.Sprintf("cL_algebraPrecision=%d.xlsx", precision)

	file1Path := filepath.Join(".", filename1)
	file2Path := filepath.Join(".", filename2)

	// 读取文件并编码为base64
	file1Data, err := os.ReadFile(file1Path)
	if err != nil {
		http.Error(w, "无法读取c10_c01文件", http.StatusInternalServerError)
		return
	}

	file2Data, err := os.ReadFile(file2Path)
	if err != nil {
		http.Error(w, "无法读取cL文件", http.StatusInternalServerError)
		return
	}

	// 创建响应
	response := DQMResponse{
		File1: base64.StdEncoding.EncodeToString(file1Data),
		File2: base64.StdEncoding.EncodeToString(file2Data),
	}

	// 返回JSON响应
	json.NewEncoder(w).Encode(response)

	// 清理生成的文件（可选）
	os.Remove(file1Path)
	os.Remove(file2Path)
}

func processHandler(w http.ResponseWriter, r *http.Request) {
	// 设置CORS头，允许跨域请求
	w.Header().Set("Access-Control-Allow-Origin", "*")
	w.Header().Set("Access-Control-Allow-Methods", "POST, OPTIONS")
	w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
	w.Header().Set("Content-Type", "application/json")

	// 处理预检请求
	if r.Method == "OPTIONS" {
		w.WriteHeader(http.StatusOK)
		return
	}

	if r.Method != http.MethodPost {
		http.Error(w, "方法不允许", http.StatusMethodNotAllowed)
		return
	}

	// 解析表单数据
	err := r.ParseMultipartForm(10 << 20) // 10MB限制
	if err != nil {
		http.Error(w, "无法解析表单", http.StatusBadRequest)
		return
	}

	// 验证用户凭据
	username := r.FormValue("username")
	password := r.FormValue("password")
	if !authenticate(username, password) {
		http.Error(w, "用户名或密码错误", http.StatusUnauthorized)
		return
	}

	// 获取阶数参数
	orderStr := r.FormValue("order")
	order, err := strconv.Atoi(orderStr)
	if err != nil || order < 2 || order > 10 {
		http.Error(w, "阶数必须是2-10的整数", http.StatusBadRequest)
		return
	}

	// 获取上传的文件
	file, header, err := r.FormFile("file")
	if err != nil {
		http.Error(w, "无法获取文件", http.StatusBadRequest)
		return
	}
	defer file.Close()

	// 创建临时目录
	tempDir, err := os.MkdirTemp("", "mesh_processing")
	if err != nil {
		http.Error(w, "无法创建临时目录", http.StatusInternalServerError)
		return
	}
	defer os.RemoveAll(tempDir)

	// 保存上传的文件
	inputPath := filepath.Join(tempDir, header.Filename)
	outFile, err := os.Create(inputPath)
	if err != nil {
		http.Error(w, "无法保存文件", http.StatusInternalServerError)
		return
	}
	defer outFile.Close()

	_, err = io.Copy(outFile, file)
	if err != nil {
		http.Error(w, "无法写入文件", http.StatusInternalServerError)
		return
	}

	// 准备输出文件路径
	outputPath := filepath.Join(tempDir, "output.xlsx")
	inputSvgPath := filepath.Join(tempDir, "input.svg")
	outputSvgPath := filepath.Join(tempDir, "output.svg")

	// 调用Mathematica脚本
	cmd := exec.Command("math", "-script", "process.m",
		strconv.Itoa(order), inputPath, outputPath, inputSvgPath, outputSvgPath)
	cmd.Dir = "." // 需要设置为process.m所在目录

	output, err := cmd.CombinedOutput()
	if err != nil {
		log.Printf("Mathematica执行错误: %s\n输出: %s", err, output)
		http.Error(w, "处理失败", http.StatusInternalServerError)
		return
	}

	// 检查输出文件是否存在
	if _, err := os.Stat(outputPath); os.IsNotExist(err) {
		http.Error(w, "未生成输出Excel文件", http.StatusInternalServerError)
		return
	}

	// 读取Excel文件并编码为base64
	excelData, err := os.ReadFile(outputPath)
	if err != nil {
		http.Error(w, "无法读取Excel文件", http.StatusInternalServerError)
		return
	}
	excelBase64 := base64.StdEncoding.EncodeToString(excelData)

	// 读取SVG文件
	inputSvg := ""
	if _, err := os.Stat(inputSvgPath); !os.IsNotExist(err) {
		svgData, err := os.ReadFile(inputSvgPath)
		if err == nil {
			inputSvg = string(svgData)
		}
	}

	outputSvg := ""
	if _, err := os.Stat(outputSvgPath); !os.IsNotExist(err) {
		svgData, err := os.ReadFile(outputSvgPath)
		if err == nil {
			outputSvg = string(svgData)
		}
	}

	// 创建响应
	response := Response{
		ExcelData: excelBase64,
		InputSvg:  inputSvg,
		OutputSvg: outputSvg,
	}

	// 返回JSON响应
	json.NewEncoder(w).Encode(response)
}
