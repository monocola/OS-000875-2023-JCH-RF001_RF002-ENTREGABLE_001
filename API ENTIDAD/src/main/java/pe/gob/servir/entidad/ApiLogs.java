package pe.gob.servir.entidad;

import org.aspectj.lang.annotation.Aspect;
import org.springframework.stereotype.Component;

@Component
@Aspect
public class ApiLogs {
//
//    private StringBuilder log = new StringBuilder();
//
//    @Pointcut("within(pe.gob.servir.entidad.service.impl.*)")
//    public void allResources() {
//        // don't need code
//    }
//
//    private String addLog(String newLog) {
//        this.log.append(newLog);
//       // int length = this.log.length();
//        /*
//        if (length > 10000) {
//            this.log.delete(900, length);
//            this.log.append(log.length()).append(" characters");
//        }
//        */
//        return this.log.toString();
//    }
//
//    private void resetLog() {
//        this.log.setLength(0);
//    }
//
//    @Before("allResources()")
//    public void apiRequestLog(JoinPoint jp) throws JsonProcessingException {
//        LogManager.getLogger(this.getClass())
//                .info("--------------------------- o ---------------------------");
//        this.resetLog();
//        this.addLog(jp.getSignature().getName());
//        this.addLog(" >>>");
//        for (Object arg : jp.getArgs()) {
//            this.addLog(String.format(" ARG: %s", arg));
//        }
//        String val = getArgsMap(jp);
//        if (!val.equals("null")){
//            this.addLog(" ARG:");
//            this.addLog(" >>>");
//            this.addLog(val);
//        }
//
//        LogManager.getLogger(this.getClass()).info(() -> this.log);
//    }
//
//    public String getArgsMap(JoinPoint pjp) throws JsonProcessingException {
//        MethodSignature signature = (MethodSignature) pjp.getSignature();
//        Map<String, Object> args = new LinkedHashMap<>();
//        String names[] = signature.getParameterNames();
//        for (int i = 0, len = names.length; i < len; i++) {
//            args.put(names[i], pjp.getArgs()[i]);
//        }
//
//
//        ReqBase reqBase = (ReqBase) args.get("request");
//        ObjectMapper objectMapper = new ObjectMapper();
//        return objectMapper.writeValueAsString(reqBase);
//    }
//
//    @AfterReturning(pointcut = "allResources()", returning = "returnValue")
//    public void apiResponseLog(JoinPoint jp, Object returnValue) {
//        this.resetLog();
//        this.addLog("<<< Return << ");
//        this.addLog(jp.getSignature().getName());
//        this.addLog(" << ");
//        if (returnValue != null) {
//            String className = returnValue.getClass().getSimpleName();
//            if (className.startsWith("Flux") || className.startsWith("Mono")) {
//                this.addLog(className);
//            } else {
//                try {
//                    this.addLog(new ObjectMapper().writeValueAsString(returnValue));
//                } catch (JsonProcessingException e) {
//                    this.addLog(returnValue.toString());
//                }
//            }
//            LogManager.getLogger(this.getClass()).info(() -> this.log);
//        } else {
//            LogManager.getLogger(this.getClass()).info(() -> this.addLog("null"));
//        }
//    }
//
//    @AfterThrowing(pointcut = "allResources()", throwing = "exception")
//    public void apiResponseExceptionLog(JoinPoint jp, Exception exception) {
//        StringWriter sw = new StringWriter();
//        PrintWriter pw = new PrintWriter(sw);
//        exception.printStackTrace(pw);
//        this.resetLog();
//        this.addLog(String.format("<<< EXCEPTION << %s: %s"
//                , exception.getClass().getSimpleName(), sw));
//        LogManager.getLogger(this.getClass()).info(() -> log);
//    }
}
