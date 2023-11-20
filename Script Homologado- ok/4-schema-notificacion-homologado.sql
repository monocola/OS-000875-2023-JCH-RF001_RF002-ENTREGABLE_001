INSERT INTO sch_notificacion.tbl_plantilla_correo
(plantilla_correo_id, codigo, nombre, html, asunto_correo, cc_correo, cco_correo, estado_registro, usuario_creacion, fecha_creacion, usuario_modificacion, fecha_modificacion)
VALUES( nextval('seq_plantilla_correo_id'), 'APR_SOL_ENT_EXT', 'Plantilla de aprobación de la solicitud de entidades externas', '<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que ha sido <strong>aprobada</strong> la solicitud externa al sistema de Gestión de Maestras de Entidad, Se estará notificando los datos de acceso al sistema. Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por SERVIR.
            </p>

            <div>Atte,</div>
            <div>Equipo Servir</div>
        </div>
        <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas sobre el sistema, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', 'Notificacion de Aprobación de Solicitud de Entidades Externas', NULL, NULL, '1', 'ADMIN', '2022-02-21', NULL, NULL);
INSERT INTO sch_notificacion.tbl_plantilla_correo
(plantilla_correo_id, codigo, nombre, html, asunto_correo, cc_correo, cco_correo, estado_registro, usuario_creacion, fecha_creacion, usuario_modificacion, fecha_modificacion)
VALUES(nextval('seq_plantilla_correo_id'), 'OBS_SOL_ENT_EXT', 'Plantilla de observación de la solicitud de entidades externas', '<html>
  <meta charset="utf-8"/>
  <head>
    <style type="text/css">         .content {         font-family: Arial;         font-size: 14px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 25px;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Arial;         font-size: 12px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
      <div class="divParagraph" style="font-size: 20px">
        <b>
          <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
        </b>
      </div>
      <div style="margin-bottom: 10px">
        <p class="divParagraph">Por medio del presente mensaje se le informa: <br>La solicitud externa de la entidad <strong>$datos.get("NOMBRE_ENTIDAD")</strong> ha sido observada por el siguiente motivo: </p>     
        <p class="divParagraph"></p>      
        <p class="divParagraph"><b>$datos.get("OBSERVACION")</b></p>    
        <p class="divParagraph">Puedes acceder al sitio web dando <a href="$datos.get("LINK_SGM")">click aquí­.</a> </p>   
              <div>Atte.</div>
              <div>Equipo Servir</div>
            </div>
            <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas sobre el sistema, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', 'Notificacion de Observación de Solicitud de Entidades Externas', NULL, NULL, '1', 'ADMIN', '2022-02-20', NULL, NULL);
INSERT INTO sch_notificacion.tbl_plantilla_correo
(plantilla_correo_id, codigo, nombre, html, asunto_correo, cc_correo, cco_correo, estado_registro, usuario_creacion, fecha_creacion, usuario_modificacion, fecha_modificacion)
VALUES( nextval('seq_plantilla_correo_id'), 'CAN_SOL_ENT_EXT', 'Plantilla de cancelación de la solicitud de entidades externas', '<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">Nos dirigimos a Ud. para comunicarle que la solicitud externa de la entidad <strong>$datos.get("NOMBRE_ENTIDAD")</strong> registrada
                en el Sistema de Gestión de maestras de la entidad, ha sido cancelada. <br>
            </p>

            <p class="divParagraph">Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por la GDCRSC en SERVIR.</p>

            <div>Atte.</div>
            <div>Equipo Servir</div>
        </div>
        <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas sobre el sistema, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', 'Notificacion de Cancelación de Solicitud de Entidades Externas', NULL, NULL, '1', 'ADMIN', '2022-02-20', NULL, NULL);
INSERT INTO sch_notificacion.tbl_plantilla_correo
(plantilla_correo_id, codigo, nombre, html, asunto_correo, cc_correo, cco_correo, estado_registro, usuario_creacion, fecha_creacion, usuario_modificacion, fecha_modificacion)
VALUES( nextval('seq_plantilla_correo_id'), 'EXI_JEFE_ORH', 'Plantilla cuando existe usuario Jefe ORH a una entidad por solicitud de entidades externas', '<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil de <strong>Gestor ORH</strong> 
				del sistema informático de SERVIR en la entidad en la que Ud. labora.
            </p>
			<p class="divParagraph">El link para el acceso al sistema es el siguiente: <strong>$datos.get("LINK_SGM")</strong></p>
            <p class="divParagraph">Nombre de Usuario: <strong>$datos.get("USUARIO")</strong></p>
            <p class="divParagraph">Contraseña: <strong>$datos.get("CLAVE")</strong></p>
            <p class="divParagraph">De presentar inconvenientes, no dude en comunicarse con el/la profesional asignado/a por SERVIR.</p>

            <div>Atentamente,</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: $datos.get("LINK_GDR")</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', 'Notificacion cuando existe el usuario Jefe ORH asociado a la entidad por Solicitud de Entidades Externas', NULL, NULL, '1', 'ADMIN', '2022-02-27', NULL, NULL);
INSERT INTO sch_notificacion.tbl_plantilla_correo
(plantilla_correo_id, codigo, nombre, html, asunto_correo, cc_correo, cco_correo, estado_registro, usuario_creacion, fecha_creacion, usuario_modificacion, fecha_modificacion)
VALUES( nextval('seq_plantilla_correo_id'), 'ASG_JEFE_ORH', 'Plantilla cuando registras un Jefe ORH a una entidad por solicitud de entidades externas', '<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil de <strong>Jefe ORH</strong> 
				del sistema informático de SERVIR en la entidad en la que Ud. labora.
            </p>
			<p class="divParagraph">El link para el acceso al sistema es el siguiente: <strong>$datos.get("LINK_SGM")</strong></p>
            <p class="divParagraph">Nombre de Usuario: <strong>$datos.get("USUARIO")</strong></p>
            <p class="divParagraph">Contraseña: <strong>$datos.get("CLAVE")</strong></p>
            <p class="divParagraph">De presentar inconvenientes, no dude en comunicarse con el/la profesional asignado/a por SERVIR.</p>

            <div>Atentamente,</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: $datos.get("LINK_GDR")</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', 'Notificacion cuando creas un usuario asociado a la entidad por Solicitud de Entidades Externas', NULL, NULL, '1', 'ADMIN', '2022-02-27', NULL, NULL);


commit;


ALTER SEQUENCE sch_notificacion.seq_plantilla_correo_id
	RESTART 32;

insert into SCH_NOTIFICACION.TBL_PLANTILLA_CORREO(plantilla_correo_id, codigo, nombre, html, asunto_correo, estado_registro, usuario_creacion,
fecha_creacion)
values (nextval('seq_plantilla_correo_id'),'REU_EVALUADOR','Plantilla para notificar al evaluador cuando el evaluado haga reuniones','<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>: </strong></div>
<div>
<p>Su evaluado/a, <strong>$datos.get("NOMBRE_USUARIO")</strong>, lo ha convocado a una reuni&oacute;n para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluado/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=$datos.get("URL_LOGO_ENTIDAD") alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>','Reunión para el establecimiento de metas', '1','ADMIN', current_date);

insert into SCH_NOTIFICACION.TBL_PLANTILLA_CORREO(plantilla_correo_id, codigo, nombre, html, asunto_correo, estado_registro, usuario_creacion,
fecha_creacion)
values (nextval('seq_plantilla_correo_id'),'REPRO_EVALUADOR','Plantilla para notificar al evaluador cuando el evaluado reprograme reuniones','<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>: </strong></div>
<div>
<p>Su evaluado/a, <strong>$datos.get("NOMBRE_USUARIO")</strong>, ha cambiado la programación de su reunión para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluado/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=$datos.get("URL_LOGO_ENTIDAD") alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>','Reprogramación de la reunión de establecimiento de metas', '1','ADMIN', current_date);

commit;

ALTER SEQUENCE sch_notificacion.seq_plantilla_correo_id
	RESTART 34;

insert into SCH_NOTIFICACION.TBL_PLANTILLA_CORREO(plantilla_correo_id, codigo, nombre, html, asunto_correo, estado_registro, usuario_creacion,
fecha_creacion)
values (nextval('seq_plantilla_correo_id'),'REU_EVALUADOR','Plantilla para notificar al evaluador cuando el evaluado haga reuniones','<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>: </strong></div>
<div>
<p>Su evaluado/a, <strong>$datos.get("NOMBRE_USUARIO")</strong>, lo ha convocado a una reuni&oacute;n para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluado/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=$datos.get("URL_LOGO_ENTIDAD") alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>','Reunión para el establecimiento de metas', '1','ADMIN', current_date);

insert into SCH_NOTIFICACION.TBL_PLANTILLA_CORREO(plantilla_correo_id, codigo, nombre, html, asunto_correo, estado_registro, usuario_creacion,
fecha_creacion)
values (nextval('seq_plantilla_correo_id'),'REPRO_EVALUADOR','Plantilla para notificar al evaluador cuando el evaluado reprograme reuniones','<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>: </strong></div>
<div>
<p>Su evaluado/a, <strong>$datos.get("NOMBRE_USUARIO")</strong>, ha cambiado la programación de su reunión para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluado/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=$datos.get("URL_LOGO_ENTIDAD") alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>','Reprogramación de la reunión de establecimiento de metas', '1','ADMIN', current_date);

commit;


INSERT INTO sch_notificacion.tbl_plantilla_correo (plantilla_correo_id, codigo, nombre, html, asunto_correo, cc_correo, cco_correo, estado_registro, usuario_creacion, fecha_creacion, usuario_modificacion, fecha_modificacion) 
VALUES(nextval('sch_notificacion.seq_plantilla_correo_id'), 'CM_SRV_CIV_GME', 'Carga Masiva Servidores Civiles GME', '<html>
   <meta charset="utf-8"/>
   <head>
      <style type="text/css">         .content {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 15px; text-align: justify;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
   </head>
   <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
      <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
         <div class="divParagraph" style="font-size: 15px">
            <b>
            <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
         </div>
         <div style="margin-bottom: 10px">
            <p class="divParagraph">Nos dirigimos a Ud. para informarle el resultado del proceso <strong>$datos.get("NOMBRE_PROCESO")</strong> con ID <strong>$datos.get("ID_PROCESO")</strong> enviado el <strong>$datos.get("FECHORA_PROCESO")</strong>:<br><br>
               <strong>$datos.get("MSG_RESULTADO")</strong>
            </p>
            <div>Atte.</div>
            <div>Equipo Servir</div>
         </div>
         <hr class="line-color">
         <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
         </div>
         <div style="float: left">
            <span class="text-footer">Para asesoría en el uso del sistema informático puede contactarse:</span>
            <br>
            <span class="text-footer">Correo electrónico: info@servir.gob.pe  - Teléfono: (01) 206-3370.</span>
         </div>
      </div>
   </body>
</html>', 'Notificacion del Resultado del Proceso de Carga Masiva de Servidores Civiles GME', NULL, NULL, '1', 'ADMIN', CURRENT_TIMESTAMP, NULL, NULL);

UPDATE sch_notificacion.tbl_plantilla_correo SET html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil <strong>Gestor GDR</strong>
                del Sistema de Gestión del Rendimiento en la entidad en que Ud. labora. <br>
            </p>
            <p class="divParagraph">Para acceder al sistema ingrese con su usuario <strong>$datos.get("USUARIO")</strong> y contraseña a través del siguiente link: <br> 
               <strong> $datos.get("LINK_GDR")</strong></p>

            <p class="divParagraph">Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por la GDCRSC en SERVIR.</p>

            <div>Atte.</div>
            <div>Equipo Servir</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: https://www.servir.gob.pe/gdr/</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>'
WHERE codigo = 'EXI_GESTOR_GDR';


commit;

UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='OBS_SOL', nombre='Plantilla cuando se observa una solicitud en talento', html='<html>
  <meta charset="utf-8"/>
  <head>
    <style type="text/css">         .content {         font-family: Arial;         font-size: 14px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 25px;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Arial;         font-size: 12px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
      <div class="divParagraph" style="font-size: 20px">
        <b>
          <span>Estimado</span>
        </b>
      </div>
      <div style="margin-bottom: 10px">
        <p class="divParagraph">Por medio del presente mensaje se le informa: <br>La solicitud de la entidad $datos.get("NOMBRE_ENTIDAD") ha sido observada por el siguiente motivo: </p>     
        <p class="divParagraph"></p>      
        <p class="divParagraph"><b>$datos.get("OBSERVACION")</b></p>    
        <p class="divParagraph">Puedes acceder al sitio web dando <a href="$datos.get("URL_SISTEMA")">click aquí.</a> </p>   
              <div>Atte.</div>
              <div>Equipo Capacita+</div>
            </div>
            <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas sobre el sistema, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', asunto_correo='Notificación Sistema Servir', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2020-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='OBS_SOL';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='INS_US_POS', nombre='Plantilla cuando creas un usuario de postulacion', html='<html>
<meta charset="utf-8"/>
<head>
    <style type="text/css">
        .content {
            font-family: Arial;
            font-size: 14px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-blue {
            color: #024487;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 25px;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Arial;
            font-size: 12px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .text-center {
            text-align:center;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }

    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">


    <div class="divParagraph text-blue text-center" style="font-size: 28px">
        <b><span>¡Bienvenido/a!</span></b>
    </div>
    <div style="margin-bottom: 10px">
        <p class="divParagraph text-center">Estamos felices que formes parte de nuestra gran familia.
        </p>

        <p class="divParagraph text-center">Para acceder al sistema <b>Postulacion</b> da <a href="http://10.240.132.31:8080/postulante/#/auth/login">click aquí,</a> e ingresa tus credenciales.
        </p>

        <p class="divParagraph text-center">Usuario: <b>$datos.get("USUARIO_LOGIN")</b> <br>
            Contraseña: <b>$datos.get("USUARIO_PASSWORD")</b>
        </p>

        <p class="lastParagraph text-center"
           style="font-size: 12px; font-style: italic"> Recuerda tienes 7 días calendario para actualizar tu contraseña.
        </p>

       
    </div>
    <hr class="line-color">

    <div style="float: left;padding-right: 5%;">
        <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
             alt=""
             height="38px"
             width="99px"
        >

    </div>


    <div>
        <span class="text-footer">Para la atención de consultas, puede contactarse a través de:</span>
        <br>
        <span class="text-footer">Correo electrónico: talento@servir.gob.pe  - Teléfono: (01) 206-3370, anexo: 2614.</span>

    </div>
</div>
</body>

</html>', asunto_correo='Bienvenido al sistema', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='INS_US_POS';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='POS_BVNDA', nombre='Plantilla de bienvenida cuando creas un  usuario de postulación', html='<html lang="es">
<meta charset="utf-8"/>
<head>

    <style type="text/css">
        .content {
            font-family: Arial;
            font-size: 16px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-blue {
            color: #0682B5;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 25px;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

		.margin-left{
			margin-left: 8px;
		}
        .text-center {
            text-align:center;
        }
		.text-left {
			text-align:left;
		}
        .text-footer {
            font-size: 11px;
            color: #666666;
        }
		.divTable{
			display: table;
			width: 100%;
		}
		.divTableRow {
			display: table-row;
		}
		.divTableCell{

			display: table-cell;
			padding: 3px 10px;
		}
		.divTableBody {
			display: table-row-group;
		}
    </style>
</head>
<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
	<div class="divTable" style="margin-bottom: 30px">
		<div class="divTableBody">
			<div class="divTableRow">
				<div class="divTableCell"><img src="http://drive.google.com/uc?id=1SYS0Juz0hNWZ0XmlZGjp0I1Ewn-T6j60"
             alt="" height="50px" width="120px"></div>
				<div class="divTableCell" style="float: right;"> <img src="http://drive.google.com/uc?id=1ttoIENv4MrZV0hcfADoagKIVFpyF5F-4"
             alt="" height="38px" width="99px"></div>
			</div>
		</div>
	</div>
    <div class="divParagraph text-blue text-left margin-left" style="font-size: 22px">
        <b><span>Hola!,</span></b>
    </div>
    <div class="margin-left" style="margin-bottom: 10px">
        <p class="divParagraph text-left">Te dejamos tu usuario y clave para que disfrutes de Talento Perú y conozcas las oportunidades a nivel nacional.
        </p>
        <p class="divParagraph text-left">usuario: <b>$datos.get("USUARIO_LOGIN")</b> <br>
            contraseña: <b>$datos.get("USUARIO_PASSWORD")</b>
        </p>
		<div class="divTable">
		<div class="divTableBody">
			<div class="divTableRow"style="display: inline-table;">
				<div class="divTableCell" style="vertical-align: middle;">
					<img src ="http://drive.google.com/uc?id=1xOA0bwa9QIAJz1KWbdo8dM_LeuwJ0P8I" alt=""  height="24px" width="24px">
				</div>
				<div class="divTableCell">
					<p class="text-left"
					   style="font-size: 12px; font-style: normal">Por seguridad tendrás que cambiar la contraseña al iniciar sesión
					</p>
				</div>
			</div>
        <div style="padding-top:40px">Atte.</div>
        <div>Servir</div>
    </div>
    <hr class="line-color">
    <div style="float: left;padding-right: 5%;">
        <img src="http://drive.google.com/uc?id=1ttoIENv4MrZV0hcfADoagKIVFpyF5F-4" alt="" height="38px" width="99px">
    </div>
    <div>
        <span class="text-footer">Para asesoría en el uso del sistema informático puede contactarse:</span>
        <br>
        <span class="text-footer">correo electrónico: talento@servir.gob.pe  - Teléfono: (01) 206-3370, anexo: 1614.</span>

    </div>
</div>
</body>
</html>', asunto_correo='Notificación Sistema Servir', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2020-02-28', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='POS_BVNDA';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='NOT_VIR', nombre='Plantilla para examen virtual', html='<html lang="es">
  <meta charset="utf-8" />
  <head>
    <style type="text/css">
      .content {
        font-family: Arial;
        font-size: 16px;
        height: auto;
        overflow: hidden;
        margin: 0 auto 0 auto;
        text-align: left;
        color: #333333;
        padding: 30px 15px 40px;
        background-color: white;
        text-align: left;
        line-height: 20px;
      }

      .text-blue {
        color: #0682b5;
      }

      .line-color {
        background-color: rgb(206, 212, 218);
        height: 1px;
        border: 0;
      }

      div {
        background: white;
      }

      .divParagraph {
        margin-bottom: 25px;
      }

      .lastParagraph {
        margin-bottom: 50px;
      }

      .margin-left {
        margin-left: 8px;
      }
      .text-center {
        text-align: center;
      }
      .text-left {
        text-align: left;
      }
      .text-footer {
        font-size: 11px;
        color: #666666;
      }
      .divTable {
        display: table;
        width: 100%;
      }
      .divTableRow {
        display: table-row;
      }
      .divTableCell {
        display: table-cell;
        padding: 3px 10px;
      }
      .divTableBody {
        display: table-row-group;
      }
    </style>
  </head>
  <body
    style="background-color: white; margin: 0 auto; width: 700px; padding: 20px"
  >
    <div
      style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px"
      class="content"
    >
      <div class="divTable" style="margin-bottom: 30px">
        <div class="divTableBody">
          <div class="divTableRow">
            <div class="divTableCell">
              <img
                src="http://drive.google.com/uc?id=1SYS0Juz0hNWZ0XmlZGjp0I1Ewn-T6j60"
                alt=""
                height="50px"
                width="120px"
              />
            </div>
            <div class="divTableCell" style="float: right">
              <img
                src="http://drive.google.com/uc?id=1ttoIENv4MrZV0hcfADoagKIVFpyF5F-4"
                alt=""
                height="38px"
                width="99px"
              />
            </div>
          </div>
        </div>
      </div>
      <div
        class="divParagraph text-blue text-left margin-left"
        style="font-size: 22px"
      >
        <b><span>Estimado,</span></b>
      </div>
      <div class="margin-left" style="margin-bottom: 10px">
        <p class="divParagraph text-left">
          Gracias por tu interés en colaborar con $datos.get("ENTIDAD") para el
          puesto de trabajo. Quedamos impresionados con tu perfil y te
          informamos que has sido seleccionado para rendir
          $datos.get("EVALUACION").
        </p>
        <p class="divParagraph text-left">
          Información:<br />
          -Fecha y Hora: <b>$datos.get("FECHA")</b><br />
          -Link: <a href="$datos.get("URL")">$datos.get("URL")</a><br />
          -Indicaciones: <b>$datos.get("INDICACIONES")</b><br />
        </p>

        <div style="padding-top: 40px">Saludos.</div>
      </div>
    </div>
  </body>
</html>
', asunto_correo='Notificación Examen Virtual', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-05-04', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='NOT_VIR';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='CREA_SOLI', nombre='Plantilla cuando creas una solicitud', html='<html> 
<meta charset="utf-8"/> 
<head>
<style type="text/css">         .content {         font-family: Arial;         font-size: 14px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 25px;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Arial;         font-size: 12px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style> 
</head> 
<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px"> 
  <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">     
    <div class="divParagraph" style="font-size: 20px">
      <span>Gracias por registrar tu solicitud</span>
    </div>
    <div class="divParagraph"><span>Hola, estimado(a)</span> 
      <p class="divParagraph">Tu solicitud será evaluada por nuestro equipo de colaboradores, y nos comunicaremos por este medio.</p>
      <p>El plazo máximo para responder a tu solicitud es de un día hábil.</p>
    </div>     
    <div>        
    <div>Atte.</div>
     <div>Equipo Servir</div>     
</div>     
<hr class="line-color">     
<div style="float: left; margin-right: 20px"><img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px"></div>     
<div style="float: left">
<span class="text-footer">Para la atención de consultas, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_CONTACTO")  - Teléfono: (01) 206-3370, anexo: 2614.</span> 
</div>
</div>
</body>
</html>', asunto_correo='Bienvenido al sistema', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2020-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='CREA_SOLI';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='RESET_PASS', nombre='Plantilla de reseteo de la clave', html='<html>
<meta charset="utf-8"/>
<head>
    <style type="text/css">
        .content {
            font-family: Arial;
            font-size: 14px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-blue {
            color: #024487;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 25px;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Arial;
            font-size: 12px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .text-center {
            text-align:center;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }

    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">


    <div class="divParagraph text-blue text-center" style="font-size: 28px">
        <b><span>¡Bienvenido/a!</span></b>
    </div>
    <div style="margin-bottom: 10px">
        <p class="divParagraph text-center">Estamos felices que formes parte de nuestra gran familia.
        </p>

        <p class="divParagraph text-center">Para acceder al sistema <b>$datos.get("NOMBRE_SISTEMA")</b> da <a href="https://www.servir.gob.pe">click aquí,</a> e ingresa tus credenciales.
        </p>

        <p class="divParagraph text-center">usuario: <b>$datos.get("USUARIO_LOGIN")</b> <br>
            contraseña: <b>$datos.get("USUARIO_PASSWORD")</b>
        </p>

        <p class="lastParagraph text-center"
           style="font-size: 12px; font-style: italic"> Recuerda tienes 7 días calendario para actualizar tu contraseña.
        </p>

        <div>Atte.</div>
        <div>Servir</div>
    </div>
    <hr class="line-color">

    <div style="float: left;padding-right: 5%;">
        <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
             alt=""
             height="38px"
             width="99px"
        >

    </div>


    <div>
        <span class="text-footer">Para asesoría en el uso del sistema informático puede contactarse:</span>
        <br>
        <span class="text-footer">correo electrónico: info@servir.gob.pe  - Teléfono: (01) 206-3370.</span>

    </div>
</div>
</body>

</html>', asunto_correo='Sistema', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2020-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='RESET_PASS';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='INS_US_CAP', nombre='Plantilla cuando creas un usuario de capacita+', html='<html>
<meta charset="utf-8"/>
<head>
    <style type="text/css">
        .content {
            font-family: Arial;
            font-size: 14px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-blue {
            color: #024487;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 25px;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Arial;
            font-size: 12px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .text-center {
            text-align:center;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }

    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">


    <div class="divParagraph text-blue text-center" style="font-size: 28px">
        <b><span>¡Bienvenido/a!</span></b>
    </div>
    <div style="margin-bottom: 10px">
        <p class="divParagraph text-center">Estamos felices que formes parte de nuestra gran familia.
        </p>

        <p class="divParagraph text-center">Para acceder al sistema <b>Capacita+</b> da <a href="http://10.240.132.49/#/capacitaciones">click aquí,</a> e ingresa tus credenciales.
        </p>

        <p class="divParagraph text-center">Usuario: <b>$datos.get("USUARIO_LOGIN")</b> <br>
            Contraseña: <b>$datos.get("USUARIO_PASSWORD")</b>
        </p>

        <p class="lastParagraph text-center"
           style="font-size: 12px; font-style: italic"> Recuerda tienes 7 días calendario para actualizar tu contraseña.
        </p>

       
    </div>
    <hr class="line-color">

    <div style="float: left;padding-right: 5%;">
        <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
             alt=""
             height="38px"
             width="99px"
        >

    </div>


    <div>
        <span class="text-footer">Para la atención de consultas sobre Capacita+, puede contactarse a través de:</span>
        <br>
        <span class="text-footer">Correo electrónico: capacitamas@servir.gob.pe  - Teléfono: (01) 206-3370, anexo: 2614.</span>

    </div>
</div>
</body>

</html>', asunto_correo='Bienvenido al sistema', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2020-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='INS_US_CAP';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='USER_EXIST', nombre='Plantilla cuando un usuario ya existe', html='<html>
  <meta charset="utf-8"/>
  <head>
    <style type="text/css">         .content {         font-family: Arial;         font-size: 14px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 25px;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Arial;         font-size: 12px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
      <div class="divParagraph" style="font-size: 20px">
        <b>
          <span>¡Bienvenido/a!</span>
        </b>
      </div>
      <div style="margin-bottom: 10px">
        <p class="divParagraph">Estamos felices que formes parte de nuestra gran familia, te informamos que <br>ya tienes un usuario asignado con anterioriad que 
        fue informado <br>al correo : $datos.get("CORREO_ELECTRONICO")</p>      
        <p class="divParagraph">Usuario : $datos.get("NRO_DOCUMENTO") <br>Contraseña : La misma contraseña de usuario que usas actualmente</p>   
                <p class="divParagraph">En caso no recuerdes tu contraseña, puedes usar la opción "Olvidé mi contraseña" ubicada en la ventana de inicio de sesión del sistema.</p>  
          
         <div>Atte.</div>
              <div>Equipo Servir</div>
            </div>
            <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', asunto_correo='Bienvenido al sistema', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2020-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='USER_EXIST';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='INS_US_ENT', nombre='Plantilla cuando creas un usuario de entidad', html='<html>
<meta charset="utf-8"/>
<head>
    <style type="text/css">
        .content {
            font-family: Arial;
            font-size: 14px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-blue {
            color: #024487;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 25px;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Arial;
            font-size: 12px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .text-center {
            text-align:center;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }

    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">


    <div class="divParagraph text-blue text-center" style="font-size: 28px">
        <b><span>¡Bienvenido/a!</span></b>
    </div>
    <div style="margin-bottom: 10px">
        <p class="divParagraph text-center">Estamos felices que formes parte de nuestra gran familia.
        </p>

        <p class="divParagraph text-center">Para acceder al sistema <b>Talento</b> da <a href="http://10.240.132.49/#/">click aquí,</a> e ingresa tus credenciales.
        </p>

        <p class="divParagraph text-center">Usuario: <b>$datos.get("USUARIO_LOGIN")</b> <br>
            Contraseña: <b>$datos.get("USUARIO_PASSWORD")</b>
        </p>

        <p class="lastParagraph text-center"
           style="font-size: 12px; font-style: italic"> Recuerda tienes 7 días calendario para actualizar tu contraseña.
        </p>

       
    </div>
    <hr class="line-color">

    <div style="float: left;padding-right: 5%;">
        <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
             alt=""
             height="38px"
             width="99px"
        >

    </div>


    <div>
        <span class="text-footer">Para la atención de consultas, puede contactarse a través de:</span>
        <br>
        <span class="text-footer">Correo electrónico: talento@servir.gob.pe  - Teléfono: (01) 206-3370, anexo: 2614.</span>

    </div>
</div>
</body>

</html>', asunto_correo='Bienvenido al sistema', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2020-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='INS_US_ENT';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='NOT_PRE', nombre='Plantilla para examen presencial', html='<html lang="es">
  <meta charset="utf-8" />
  <head>
    <style type="text/css">
      .content {
        font-family: Arial;
        font-size: 16px;
        height: auto;
        overflow: hidden;
        margin: 0 auto 0 auto;
        text-align: left;
        color: #333333;
        padding: 30px 15px 40px;
        background-color: white;
        text-align: left;
        line-height: 20px;
      }

      .text-blue {
        color: #0682b5;
      }

      .line-color {
        background-color: rgb(206, 212, 218);
        height: 1px;
        border: 0;
      }

      div {
        background: white;
      }

      .divParagraph {
        margin-bottom: 25px;
      }

      .lastParagraph {
        margin-bottom: 50px;
      }

      .margin-left {
        margin-left: 8px;
      }
      .text-center {
        text-align: center;
      }
      .text-left {
        text-align: left;
      }
      .text-footer {
        font-size: 11px;
        color: #666666;
      }
      .divTable {
        display: table;
        width: 100%;
      }
      .divTableRow {
        display: table-row;
      }
      .divTableCell {
        display: table-cell;
        padding: 3px 10px;
      }
      .divTableBody {
        display: table-row-group;
      }
    </style>
  </head>
  <body
    style="background-color: white; margin: 0 auto; width: 700px; padding: 20px"
  >
    <div
      style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px"
      class="content"
    >
      <div class="divTable" style="margin-bottom: 30px">
        <div class="divTableBody">
          <div class="divTableRow">
            <div class="divTableCell">
              <img
                src="http://drive.google.com/uc?id=1SYS0Juz0hNWZ0XmlZGjp0I1Ewn-T6j60"
                alt=""
                height="50px"
                width="120px"
              />
            </div>
            <div class="divTableCell" style="float: right">
              <img
                src="http://drive.google.com/uc?id=1ttoIENv4MrZV0hcfADoagKIVFpyF5F-4"
                alt=""
                height="38px"
                width="99px"
              />
            </div>
          </div>
        </div>
      </div>
      <div
        class="divParagraph text-blue text-left margin-left"
        style="font-size: 22px"
      >
        <b><span>Estimado,</span></b>
      </div>
      <div class="margin-left" style="margin-bottom: 10px">
        <p class="divParagraph text-left">
          Gracias por tu interés en colaborar con $datos.get("ENTIDAD") para el
          puesto de trabajo. Quedamos impresionados con tu perfil y te
          informamos que has sido seleccionado para rendir
          $datos.get("EVALUACION").
        </p>
        <p class="divParagraph text-left">
          Información:<br />
          -Fecha y Hora: <b>$datos.get("FECHA")</b><br />
          -Lugar: <b>$datos.get("URL")</b><br />
          -Referencia: <b>$datos.get("REFERENCIA")</b><br />
          -Indicaciones: <b>$datos.get("INDICACIONES")</b><br />
        </p>

        <div style="padding-top: 40px">Saludos.</div>
      </div>
    </div>
  </body>
</html>
', asunto_correo='Notificación Examen Presencial', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-05-04', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='NOT_PRE';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='OBS_META', nombre='Plantilla cuando se observa una meta en Planificacion', html='<html>
  <meta charset="utf-8"/>
  <head>
    <style type="text/css">         .content {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 15px; text-align: justify;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
      <div class="divParagraph" style="font-size: 15px">
        <b>
          <span>Estimado/a $datos.get("NOMBRE_EVALUADOR"):</span>
        </b>
      </div>
      <div style="margin-bottom: 10px">
        <p class="divParagraph">Por medio del presente comunicarle que una de las metas asignadas a su evaluado/a $datos.get("NOMBRE_EVALUADO"), correspondiente al ciclo $datos.get("ANIO_CICLO"), ha sido observado. <br><br>
		Sustento de la observación:<br>
		$datos.get("SUSTENTO_OBSERVACION")		
		</p>
        <p class="divParagraph">Al respecto, corresponde a la oficina de Recursos Humanos o la que haga sus veces derivar al/a la superior jerárquico/a del/de la evaluador/a, en un plazo máximo de dos (2) días hábiles, la solicitud de disconformidad del/de la evaluado/a y la información correspondiente al numeral 7.2 de la Directiva del subsistema de Gestión del Rendimiento.</p>
        <p class="divParagraph">Asimismo, el/la superior jerárquico/a del evaluador/a, en un plazo máximo de tres (3) días hábiles de recibido el requerimiento por la Oficina de recursos humanos o la que haga sus veces, confirma o modifica la meta observada, con la potestad de citar a evaluador/a y evaluado/a para esta actividad. La decisión del/de la superior jerárquico del/de la evaluador/a es irrecurrible y debe formalizarse en el formato dispuesto por SERVIR.</p>
		<p class="divParagraph">Cabe mencionar que la presente notificación ha sido remitida en copia a la oficina de recursos o la que haga sus veces, a fin de que se tomen las acciones correspondientes. Cualquier consulta, no dude en comunicarse con su gestor de Gestión del Rendimiento en la entidad.</p>

         <div>Atte.</div>
              <div>Equipo Servir</div>
            </div>
            <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', asunto_correo='Notificación Observación de Metas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='OBS_META';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='REGISTRO_RECTOR', nombre='Registro de Usuario Rector GDCRSC', html='<html>
  <meta charset="utf-8"/>
  <head>
    <style type="text/css">         .content {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 15px; text-align: justify;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
      <div class="divParagraph" style="font-size: 15px">
        <b>
          <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
        </b>
      </div>
      <div style="margin-bottom: 10px">
        <p class="divParagraph">Nos dirigimos a Ud. para comunicarle que ha sido registrado como usuario Rector GDCRSC del Sistema de Gestión del Rendimiento en la entidad en que Ud. labora.<br><br>
		Para acceder al sistema ingrese con su usuario y contraseña a través del siguiente link: <strong>$datos.get("LINK_GDR")</strong>				
		</p>       
		<p class="divParagraph">De presentar inconvenientes, no dude en comunicarse con el equipo de GDR en la GDCRSC.</p>

         <div>Atte.</div>
              <div>Equipo Servir</div>
            </div>
            <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', asunto_correo='Notificación de Registro Usuario Rector GDCRSC', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='REGISTRO_RECTOR';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='REG_GESTOR_GDR', nombre='Plantilla de registro de Gestor GDR', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil <strong>Gestor GDR</strong>
                del Sistema de Gestión del Rendimiento en la entidad en que Ud. labora. <br><br>
                El link para el acceso al sistema es el siguiente: <strong>$datos.get("LINK_GDR")</strong>
            </p>
            <p class="divParagraph">Nombre de Usuario: <strong>$datos.get("USUARIO")</strong></p>
            <p class="divParagraph">Contraseña: <strong>$datos.get("CLAVE")</strong></p>
            <p class="divParagraph">Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por la GDCRSC en SERVIR.</p>

            <div>Atte.</div>
            <div>Equipo Servir</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: https://www.servir.gob.pe/gdr/</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', asunto_correo='Notificación de Registro de Gestor GDR', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-21', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='REG_GESTOR_GDR';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='EXI_GESTOR_GDR', nombre='Plantilla de existencia de Gestor GDR', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil <strong>Gestor GDR</strong>
                del Sistema de Gestión del Rendimiento en la entidad en que Ud. labora. <br>
            </p>
            <p class="divParagraph">Para acceder al sistema ingrese con su usuario <strong>$datos.get("USUARIO")</strong> y contraseña a través del siguiente link: <br> 
               <strong> $datos.get("LINK_GDR")</strong></p>

            <p class="divParagraph">Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por la GDCRSC en SERVIR.</p>

            <div>Atte.</div>
            <div>Equipo Servir</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: https://www.servir.gob.pe/gdr/</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', asunto_correo='Notificación de existencia de Gestor GDR', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-21', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='EXI_GESTOR_GDR';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='REG_SOL_ENT_EXT', nombre='Plantilla de registro de solicitud de entidades externas', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que su solicitud de registro en el sistema informático de SERVIR 
				ha sido recibida. Una vez que validemos la información de su solicitud, se estará notificando los datos 
				de acceso al sistema. Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por SERVIR.
            </p>

            <div>Atte,</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: $datos.get("LINK_GDR")</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', asunto_correo='Notificación de Solicitud de Registro de Entidades Externas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-26', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='REG_SOL_ENT_EXT';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='CANC_GESTOR_GDR', nombre='Plantilla de cancelacion de cuenta de Gestor GDR', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil <strong>Gestor GDCRSC</strong>
                del Sistema de Gestión del Rendimiento en la entidad en que Ud. labora, ha sido cancelada. <br>
            </p>

            <p class="divParagraph">Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por la GDCRSC en SERVIR.</p>

            <div>Atte.</div>
            <div>Equipo Servir</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: https://www.servir.gob.pe/gdr/</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', asunto_correo='Notificación de cancelación de cuenta de Gestor GDR', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-21', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='CANC_GESTOR_GDR';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='NOT_REU', nombre='Reunión para el establecimiento de metas', html='<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>: </strong></div>
<div>
<p>Su evaluador/a, <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>, lo ha convocado a una reuni&oacute;n para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluador/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=$datos.get("URL_LOGO_ENTIDAD") alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>', asunto_correo='Reunión para el establecimiento de metas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-27', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='NOT_REU';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='EXI_GESTOR_ORH', nombre='Plantilla de Gestor ORH existente', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil de <strong>Gestor ORH</strong> 
				del sistema informático de SERVIR en la entidad en la que Ud. labora.
            </p>
            <p class="divParagraph">Para acceder al sistema ingrese con su usuario <strong>$datos.get("USUARIO")</strong> y contraseña a través del siguiente link:  </p>
            <p class="divParagraph"> <strong>$datos.get("LINK_SGM")</strong></p> <br>
			
            <p class="divParagraph">De presentar inconvenientes, no dude en comunicarse con el/la profesional asignado/a por SERVIR..</p>

            <div>Atentamente,</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: $datos.get("LINK_GDR")</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', asunto_correo='Notificación de Gestor ORH existente', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-28', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='EXI_GESTOR_ORH';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='NOT_REP_REU', nombre='Reprogramación de la reunión de establecimiento de metas', html='<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>: </strong></div>
<div>
<p>Su evaluador/a, <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>, ha cambiado la programación de su reunión para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluador/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=$datos.get("URL_LOGO_ENTIDAD") alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>', asunto_correo='Reprogramación de la reunión de establecimiento de metas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-27', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE  codigo='NOT_REP_REU';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='ASG_GESTOR_ORH', nombre='Plantilla de asignacion de Gestor ORH', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil de <strong>Gestor ORH</strong> 
				del sistema informático de SERVIR en la entidad en la que Ud. labora.
            </p>
			<p class="divParagraph">El link para el acceso al sistema es el siguiente: <strong>$datos.get("LINK_SGM")</strong></p>
            <p class="divParagraph">Nombre de Usuario: <strong>$datos.get("USUARIO")</strong></p>
            <p class="divParagraph">Contraseña: <strong>$datos.get("CLAVE")</strong></p>
            <p class="divParagraph">De presentar inconvenientes, no dude en comunicarse con el/la profesional asignado/a por SERVIR.</p>

            <div>Atentamente,</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: $datos.get("LINK_GDR")</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', asunto_correo='Notificación de Registro de Gestor ORH', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-28', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='ASG_GESTOR_ORH';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='NOT_RENV_REU', nombre='Recordatorio de la reunión de establecimiento de metas', html='<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>: </strong></div>
<div>
<p>Su evaluador/a, <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>, lo ha convocado a una reuni&oacute;n para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluador/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=@{$datos.get("URL_LOGO_ENTIDAD")} alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>', asunto_correo='Recordatorio de la reunión de establecimiento de metas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-10-27', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='NOT_RENV_REU';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='APR_SOL_ENT_EXT', nombre='Plantilla de aprobación de la solicitud de entidades externas', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que ha sido <strong>aprobada</strong> la solicitud externa al sistema de Gestión de Maestras de Entidad, Se estará notificando los datos de acceso al sistema. Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por SERVIR.
            </p>

            <div>Atte,</div>
            <div>Equipo Servir</div>
        </div>
        <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas sobre el sistema, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', asunto_correo='Notificación de Aprobación de Solicitud de Entidades Externas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2022-02-21', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='APR_SOL_ENT_EXT';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='OBS_SOL_ENT_EXT', nombre='Plantilla de observación de la solicitud de entidades externas', html='<html>
  <meta charset="utf-8"/>
  <head>
    <style type="text/css">         .content {         font-family: Arial;         font-size: 14px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 25px;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Arial;         font-size: 12px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
      <div class="divParagraph" style="font-size: 20px">
        <b>
          <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
        </b>
      </div>
      <div style="margin-bottom: 10px">
        <p class="divParagraph">Por medio del presente mensaje se le informa: <br>La solicitud externa de la entidad <strong>$datos.get("NOMBRE_ENTIDAD")</strong> ha sido observada por el siguiente motivo: </p>     
        <p class="divParagraph"></p>      
        <p class="divParagraph"><b>$datos.get("OBSERVACION")</b></p>    
        <p class="divParagraph">Puedes acceder al sitio web dando <a href="$datos.get("LINK_SGM")">click aquí­.</a> </p>   
              <div>Atte.</div>
              <div>Equipo Servir</div>
            </div>
            <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas sobre el sistema, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', asunto_correo='Notificación de Observación de Solicitud de Entidades Externas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2022-02-20', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='OBS_SOL_ENT_EXT';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='CAN_SOL_ENT_EXT', nombre='Plantilla de cancelación de la solicitud de entidades externas', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">Nos dirigimos a Ud. para comunicarle que la solicitud externa de la entidad <strong>$datos.get("NOMBRE_ENTIDAD")</strong> registrada
                en el Sistema de Gestión de maestras de la entidad, ha sido cancelada. <br>
            </p>

            <p class="divParagraph">Cualquier consulta, no dude en comunicarse con el/la profesional asignado/a por la GDCRSC en SERVIR.</p>

            <div>Atte.</div>
            <div>Equipo Servir</div>
        </div>
        <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas sobre el sistema, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', asunto_correo='Notificación de Cancelación de Solicitud de Entidades Externas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2022-02-20', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='CAN_SOL_ENT_EXT';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='CREA_USUA', nombre='Plantilla cuando creas un usuario', html='<html>
<meta charset="utf-8"/>
<head>
    <style type="text/css">
        .content {
            font-family: Arial;
            font-size: 14px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-blue {
            color: #024487;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 25px;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Arial;
            font-size: 12px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .text-center {
            text-align:center;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }

    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">


    <div class="divParagraph text-blue text-center" style="font-size: 28px">
        <b><span>¡Bienvenido/a!</span></b>
    </div>
    <div style="margin-bottom: 10px">
        <p class="divParagraph text-center">Estamos felices que formes parte de nuestra gran familia.
        </p>

        <p class="divParagraph text-center">Para acceder al sistema <b>$datos.get("NOMBRE_SISTEMA")</b> da <a href="$datos.get("URL_SISTEMA")">click aquí,</a> e ingresa tus credenciales.
        </p>

        <p class="divParagraph text-center">usuario: <b>$datos.get("USUARIO_LOGIN")</b> <br>
            contraseña: <b>$datos.get("USUARIO_PASSWORD")</b>
        </p>

        <p class="lastParagraph text-center"
           style="font-size: 12px; font-style: italic"> Recuerda tienes 7 días calendario para actualizar tu contraseña.
        </p>

        <div>Atte.</div>
        <div>Servir</div>
    </div>
    <hr class="line-color">

    <div style="float: left">
        <img src="http://drive.google.com/uc?id=1JRHzVU5WcaKzSBZJLPn_v2k1ruHqqojD" alt="logo-servir" title = "logo-servir" height="38px" width="99px" >
    </div>

    <div>
        <span class="text-footer">Para asesoría en el uso del sistema informático puede contactarse:</span>
        <br>
        <span class="text-footer">correo electrónico: info@servir.gob.pe  - Teléfono: (01) 206-3370.</span>

    </div>
</div>
</body>

</html>', asunto_correo='Bienvenido al sistema', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2020-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='CREA_USUA';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='EXI_JEFE_ORH', nombre='Plantilla cuando existe usuario Jefe ORH a una entidad por solicitud de entidades externas', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil de <strong>JEFE ORH</strong> 
				del sistema informático de SERVIR en la entidad en la que Ud. labora.
            </p>
            <p class="divParagraph">Para acceder al sistema ingrese con su usuario <strong>$datos.get("USUARIO")</strong> y contraseña a través del siguiente link:  </p>
            <p class="divParagraph"> <strong>$datos.get("LINK_SGM")</strong></p> <br>
			
            <p class="divParagraph">De presentar inconvenientes, no dude en comunicarse con el/la profesional asignado/a por SERVIR..</p>

            <div>Atentamente,</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: $datos.get("LINK_GDR")</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', asunto_correo='Notificación cuando existe el usuario Jefe ORH asociado a la entidad por Solicitud de Entidades Externas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2022-02-27', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='EXI_JEFE_ORH';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='ASIG_RECTOR', nombre='Asignación de usuario rector GDCRSC', html='<html>
  <meta charset="utf-8"/>
  <head>
    <style type="text/css">         .content {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 15px; text-align: justify;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
      <div class="divParagraph" style="font-size: 15px">
        <b>
          <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
        </b>
      </div>
      <div style="margin-bottom: 10px">
        <p class="divParagraph">Nos dirigimos a Ud. para comunicarle que ha sido registrado como usuario Rector GDCRSC del Sistema de Gestión del Rendimiento en la entidad en que Ud. labora. <br><br>
		El link para el acceso al sistema es el siguiente: <strong>$datos.get("LINK_GDR")</strong>				
		</p>		
        <p class="divParagraph">Nombre de Usuario: <strong>$datos.get("USUARIO")</strong></p>
        <p class="divParagraph">Contraseña: <strong>$datos.get("CLAVE")</strong></p>
		<p class="divParagraph">De presentar inconvenientes, no dude en comunicarse con el equipo de GDR en la GDCRSC.</p>

         <div>Atte.</div>
              <div>Equipo Servir</div>
            </div>
            <hr class="line-color">
              <div style="float: left; margin-right: 20px">
                <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
                </div>
                <div style="float: left">
                     <span class="text-footer">Para la atención de consultas, puede contactarse a través de:</span>
<br>
<span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR")  - Teléfono: (01) 206-3370, anexo: 2614.</span>
                    </div>
                  </div>
                </body>
              </html>', asunto_correo='Notificación de Asignación Usuario Rector GDCRSC', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2021-05-07', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='ASIG_RECTOR';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='ASG_JEFE_ORH', nombre='Plantilla cuando registras un Jefe ORH a una entidad por solicitud de entidades externas', html='<html>
<meta charset="utf-8" />

<head>
    <style type="text/css">
        .content {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            padding: 30px 15px 40px;
            background-color: white;
            text-align: left;
            line-height: 20px;
        }

        .text-green {
            color: #06B264;
        }

        .text-gray {
            color: #666666;
        }

        .line-color {
            background-color: rgb(206, 212, 218);
            height: 1px;
            border: 0;
        }

        div {
            background: white;
        }

        .divParagraph {
            margin-bottom: 15px;
            text-align: justify;
        }

        .lastParagraph {
            margin-bottom: 50px;
        }

        .contentToSend {
            font-family: Calibri;
            font-size: 11px;
            height: auto;
            overflow: hidden;
            margin: 0 auto 0 auto;
            text-align: left;
            color: #333333;
            background-color: white;
            text-align: left;
        }

        .positionData {
            position: relative;
            left: 5px;
        }

        .positionNewCC {
            position: relative;
            left: 30px;
        }

        .text-footer {
            font-size: 11px;
            color: #666666;
        }
    </style>
</head>

<body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
    <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
        <div class="divParagraph" style="font-size: 15px">
            <b>
                <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
        </div>
        <div style="margin-bottom: 10px">
            <p class="divParagraph">
				Nos dirigimos a Ud. para comunicarle que ha sido registrado con el perfil de <strong>Jefe ORH</strong> 
				del sistema informático de SERVIR en la entidad en la que Ud. labora.
            </p>
			<p class="divParagraph">El link para el acceso al sistema es el siguiente: <strong>$datos.get("LINK_SGM")</strong></p>
            <p class="divParagraph">Nombre de Usuario: <strong>$datos.get("USUARIO")</strong></p>
            <p class="divParagraph">Contraseña: <strong>$datos.get("CLAVE")</strong></p>
            <p class="divParagraph">De presentar inconvenientes, no dude en comunicarse con el/la profesional asignado/a por SERVIR.</p>

            <div>Atentamente,</div>
        </div>
        <hr class="line-color">
        <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png"
                alt="" height="38px" width="99px">
        </div>
        <div style="float: left">
            <span class="text-footer">Página web de GDR: $datos.get("LINK_GDR")</span><br>
            <span class="text-footer">Correo electrónico: $datos.get("CORREO_SERVIR") </span> <br>
            <span class="text-footer">Teléfono: (01) 206-3370, anexos: 2622 – 2616 – 2621 – 2624 – 2620.</span>
        </div>
    </div>
</body>

</html>', asunto_correo='Notificación cuando creas un usuario asociado a la entidad por Solicitud de Entidades Externas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2022-02-27', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='ASG_JEFE_ORH';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='REPRO_EVALUADOR', nombre='Plantilla para notificar al evaluador cuando el evaluado reprograme reuniones', html='<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>: </strong></div>
<div>
<p>Su evaluado/a, <strong>$datos.get("NOMBRE_USUARIO")</strong>, ha cambiado la programación de su reunión para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluado/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=$datos.get("URL_LOGO_ENTIDAD") alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>', asunto_correo='Reprogramación de la reunión de establecimiento de metas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2023-02-21', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='REPRO_EVALUADOR';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='REU_EVALUADOR', nombre='Plantilla para notificar al evaluador cuando el evaluado haga reuniones', html='<html>
  <meta charset="utf-8"/>
  <head>
  </head>
  <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
<div><strong> Estimado/a <strong>$datos.get("NOMBRE_COMPLETO_DEL_EVALUADOR")</strong>: </strong></div>
<div>
<p>Su evaluado/a, <strong>$datos.get("NOMBRE_USUARIO")</strong>, lo ha convocado a una reuni&oacute;n para el establecimiento de metas del ciclo $datos.get("ANO_DEL_CICLO"), seg&uacute;n el siguiente detalle:<br /><br /></p>
<p style="line-height: 50%;">D&iacute;a: <strong>$datos.get("FECHA")</strong></p>
<p style="line-height: 50%;">Hora: <strong>$datos.get("HORA")</strong></p>
<p style="line-height: 50%;">Duraci&oacute;n aproximada: <strong>$datos.get("DURACION")</strong></p>
<p style="line-height: 50%;">&nbsp;</p>
<p>De presentar inconvenientes, agradeceremos comunicar oportunamente a su evaluado/a.</p>
<div>Atentamente</div>
</div>
<div>&nbsp;</div>
<div>
<table style="border-collapse: collapse; width: 100%;" border="1">
<tbody>
<tr>
<td rowspan="2">
<div>
<img src=$datos.get("URL_LOGO_ENTIDAD") alt="" height="38px" width="99px">
</div>
</td>
<td style="width: 50%;">
<p>Correo electr&oacute;nico: $datos.get("CORREO_DE_CONSULTAS_ENTIDAD")</p>
<p>Tel&eacute;fono: $datos.get("TELEFONO"), anexo: $datos.get("ANEXO").</p>
</td>
</tr>
</tbody>
</table>
</div>
</body>
</html>', asunto_correo='Reunión para el establecimiento de metas', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2023-02-21', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='REU_EVALUADOR';
UPDATE sch_notificacion.tbl_plantilla_correo
SET codigo='CM_SRV_CIV_GME', nombre='Carga Masiva Servidores Civiles GME', html='<html>
   <meta charset="utf-8"/>
   <head>
      <style type="text/css">         .content {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         padding: 30px 15px 40px;         background-color: white;         text-align: left;         line-height: 20px;     }     .text-green {         color: #06B264;     }     .text-gray {         color: #666666;     }     .line-color {         background-color: rgb(206, 212, 218);         height: 1px;         border: 0;     }     div {         background: white;     }     .divParagraph {         margin-bottom: 15px; text-align: justify;     }     .lastParagraph {         margin-bottom: 50px;     }     .contentToSend {         font-family: Calibri;         font-size: 11px;         height: auto;         overflow: hidden;         margin: 0 auto 0 auto;         text-align: left;         color: #333333;         background-color: white;         text-align: left;     }     .positionData {         position: relative;         left: 5px;     }     .positionNewCC {         position: relative;         left: 30px;     }     .text-footer {         font-size: 11px;         color: #666666;     }     </style>
   </head>
   <body style="background-color: white; margin: 0 auto; width: 700px; padding: 20px">
      <div style="border: 1pt solid rgb(206, 212, 218); border-radius: 10px" class="content">
         <div class="divParagraph" style="font-size: 15px">
            <b>
            <span>Estimado/a <strong>$datos.get("NOMBRE_USUARIO")</strong>:</span>
            </b>
         </div>
         <div style="margin-bottom: 10px">
            <p class="divParagraph">Nos dirigimos a Ud. para informarle el resultado del proceso <strong>$datos.get("NOMBRE_PROCESO")</strong> con ID <strong>$datos.get("ID_PROCESO")</strong> enviado el <strong>$datos.get("FECHORA_PROCESO")</strong>:<br><br>
               <strong>$datos.get("MSG_RESULTADO")</strong>
            </p>
            <div>Atte.</div>
            <div>Equipo Servir</div>
         </div>
         <hr class="line-color">
         <div style="float: left; margin-right: 20px">
            <img src="https://www.servir.gob.pe/wp-content/themes/Shigo%20Servir%20Theme%20v1/sjci/img/logo-color.png" alt="" height="38px" width="99px">
         </div>
         <div style="float: left">
            <span class="text-footer">Para asesoría en el uso del sistema informático puede contactarse:</span>
            <br>
            <span class="text-footer">Correo electrónico: info@servir.gob.pe  - Teléfono: (01) 206-3370.</span>
         </div>
      </div>
   </body>
</html>', asunto_correo='Notificación del Resultado del Proceso de Carga Masiva de Servidores Civiles GME', cc_correo=NULL, cco_correo=NULL, estado_registro='1', usuario_creacion='ADMIN', fecha_creacion='2023-04-02', usuario_modificacion=NULL, fecha_modificacion=NULL
WHERE codigo='CM_SRV_CIV_GME';


commit;