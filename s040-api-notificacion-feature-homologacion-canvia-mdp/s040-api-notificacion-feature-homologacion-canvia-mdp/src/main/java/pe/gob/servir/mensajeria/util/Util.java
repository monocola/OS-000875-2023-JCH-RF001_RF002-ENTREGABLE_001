package pe.gob.servir.mensajeria.util;

import java.io.StringWriter;
import java.util.Map;

import org.apache.velocity.VelocityContext;
import org.apache.velocity.app.VelocityEngine;

public class Util {
    public static String velocityEngine(String templateEmail, Map<String, Object> parametros) {
        StringWriter out = new StringWriter();
        VelocityEngine velocityEngine = new VelocityEngine();
        VelocityContext context = new VelocityContext();
        context.put("datos", parametros);
        velocityEngine.evaluate(context, out, "log tag name", templateEmail);
        return out.toString();
    }
}
