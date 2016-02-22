/*     */ package game;
/*     */ 
/*     */ import java.io.BufferedReader;
/*     */ import java.io.File;
/*     */ import java.io.FileOutputStream;
/*     */ import java.io.InputStream;
/*     */ import java.io.InputStreamReader;
/*     */ import java.io.OutputStream;
/*     */ import java.io.PrintStream;
/*     */ import java.net.URISyntaxException;
/*     */ import java.net.URL;
/*     */ import java.security.CodeSource;
/*     */ import java.security.ProtectionDomain;
/*     */ import java.util.ArrayList;
/*     */ import java.util.Enumeration;
/*     */ import java.util.Random;
/*     */ import java.util.StringTokenizer;
/*     */ import java.util.jar.Attributes;
/*     */ import java.util.jar.JarEntry;
/*     */ import java.util.jar.JarFile;
/*     */ import java.util.jar.Manifest;
/*     */ 
/*     */ public class Launcher
/*     */ {
/*     */   public Launcher()
/*     */     throws Exception
/*     */   {
/*  55 */     File file = getCodeSourceLocation();
/*     */     
/*  57 */     String nativeDirectory = getNativeDirectory();
/*     */     
/*  59 */     String mainClass = getMainClass(file);
/*  60 */     String vmArgs = getVmArgs(file);
/*     */     try
/*     */     {
/*  63 */       extractNatives(file, nativeDirectory);
/*     */       
/*     */ 
/*     */ 
/*     */ 
/*     */ 
/*     */ 
/*     */ 
/*     */ 
/*  72 */       ArrayList arguments = new ArrayList();
/*     */       
/*     */ 
/*  75 */       String javaPath = System.getProperty("java.home") + File.separator + "bin" + File.separator + "java";
/*  76 */       arguments.add(javaPath);
/*     */       
/*  78 */       StringTokenizer vmArgsToken = new StringTokenizer(vmArgs, " ");
/*  79 */       int count = vmArgsToken.countTokens();
/*  80 */       for (int i = 0; i < count; i++) {
/*  81 */         arguments.add(vmArgsToken.nextToken());
/*     */       }
/*  83 */       arguments.add("-cp");
/*  84 */       arguments.add(file.getAbsoluteFile().toString());
/*  85 */       arguments.add("-Djava.library.path=" + nativeDirectory);
/*  86 */       arguments.add(mainClass);
/*     */       
/*     */ 
/*     */ 
/*     */ 
/*     */ 
/*     */ 
/*     */ 
/*  94 */       ProcessBuilder processBuilder = new ProcessBuilder(arguments);
/*  95 */       processBuilder.redirectErrorStream(true);
/*  96 */       Process process = processBuilder.start();
/*     */       
/*  98 */       writeConsoleOutput(process);
/*     */       
/* 100 */       process.waitFor();
/*     */     }
/*     */     finally
/*     */     {
/* 103 */       deleteNativeDirectory(nativeDirectory);
/*     */     }
/*     */   }
/*     */   
/*     */   public void writeConsoleOutput(Process process)
/*     */     throws Exception
/*     */   {
/* 108 */     InputStream is = process.getInputStream();
/* 109 */     InputStreamReader isr = new InputStreamReader(is);
/* 110 */     BufferedReader br = new BufferedReader(isr);
/*     */     String line;
/* 113 */     while ((line = br.readLine()) != null)
/*     */     {
/* 114 */       System.out.println(line);
/*     */     }
/*     */   }
/*     */   
/*     */   public void extractNatives(File file, String nativeDirectory)
/*     */     throws Exception
/*     */   {
/* 120 */     JarFile jarFile = new JarFile(file, false);
/* 121 */     Enumeration entities = jarFile.entries();
/* 123 */     while (entities.hasMoreElements())
/*     */     {
/* 124 */       JarEntry entry = (JarEntry)entities.nextElement();
/* 126 */       if ((!entry.isDirectory()) && (entry.getName().indexOf('/') == -1)) {
/* 130 */         if (isNativeFile(entry.getName()))
/*     */         {
/* 134 */           InputStream in = jarFile.getInputStream(jarFile.getEntry(entry.getName()));
/* 135 */           OutputStream out = new FileOutputStream(nativeDirectory + File.separator + entry.getName());
/*     */           
/*     */ 
/* 138 */           byte[] buffer = new byte[65536];
/*     */           int bufferSize;
/* 140 */           while ((bufferSize = in.read(buffer, 0, buffer.length)) != -1)
/*     */           {
/* 141 */             out.write(buffer, 0, bufferSize);
/*     */           }
/* 144 */           in.close();
/* 145 */           out.close();
/*     */         }
/*     */       }
/*     */     }
/* 148 */     jarFile.close();
/*     */   }
/*     */   
/*     */   public boolean isNativeFile(String entryName)
/*     */   {
/* 152 */     String osName = System.getProperty("os.name");
/* 153 */     String name = entryName.toLowerCase();
/* 155 */     if (osName.startsWith("Win"))
/*     */     {
/* 156 */       if (name.endsWith(".dll")) {
/* 157 */         return true;
/*     */       }
/*     */     }
/* 159 */     else if (osName.startsWith("Linux"))
/*     */     {
/* 160 */       if (name.endsWith(".so")) {
/* 161 */         return true;
/*     */       }
/*     */     }
/* 163 */     else if (((osName.startsWith("Mac")) || (osName.startsWith("Darwin"))) && (
/* 164 */       (name.endsWith(".jnilib")) || (name.endsWith(".dylib")))) {
/* 165 */       return true;
/*     */     }
/* 169 */     return false;
/*     */   }
/*     */   
/*     */   public String getNativeDirectory()
/*     */   {
/* 173 */     String nativeDir = System.getProperty("deployment.user.cachedir");
/* 175 */     if ((nativeDir == null) || (System.getProperty("os.name").startsWith("Win"))) {
/* 176 */       nativeDir = System.getProperty("java.io.tmpdir");
/*     */     }
/* 179 */     nativeDir = nativeDir + File.separator + "natives" + new Random().nextInt();
/*     */     
/* 181 */     File dir = new File(nativeDir);
/* 184 */     if (!dir.exists()) {
/* 185 */       dir.mkdirs();
/*     */     }
/* 188 */     return nativeDir;
/*     */   }
/*     */   
/*     */   public void deleteNativeDirectory(String directoryName)
/*     */   {
/* 192 */     File directory = new File(directoryName);
/*     */     
/* 194 */     File[] files = directory.listFiles();
/* 195 */     for (File file : files) {
/* 196 */       file.delete();
/*     */     }
/* 199 */     directory.delete();
/*     */   }
/*     */   
/*     */   public String getMainClass(File file)
/*     */     throws Exception
/*     */   {
/* 203 */     JarFile jarFile = new JarFile(file);
/* 204 */     Manifest manifest = jarFile.getManifest();
/* 205 */     Attributes attribute = manifest.getMainAttributes();
/*     */     
/* 207 */     return attribute.getValue("Launcher-Main-Class");
/*     */   }
/*     */   
/*     */   public String getVmArgs(File file)
/*     */     throws Exception
/*     */   {
/* 211 */     JarFile jarFile = new JarFile(file);
/* 212 */     Manifest manifest = jarFile.getManifest();
/* 213 */     Attributes attribute = manifest.getMainAttributes();
/*     */     
/* 215 */     return attribute.getValue("Launcher-VM-Args");
/*     */   }
/*     */   
/*     */   public File getCodeSourceLocation()
/*     */   {
/*     */     try
/*     */     {
/* 225 */       return new File(Launcher.class.getProtectionDomain().getCodeSource().getLocation().toURI());
/*     */     }
/*     */     catch (URISyntaxException e)
/*     */     {
/* 227 */       e.printStackTrace();
/*     */     }
/* 230 */     return null;
/*     */   }
/*     */   
/*     */   public static void main(String[] args)
/*     */     throws Exception
/*     */   {
/* 234 */     new Launcher();
/*     */   }
/*     */ }