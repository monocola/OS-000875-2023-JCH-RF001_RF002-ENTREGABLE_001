ALTER TABLE sch_planificacion.tbl_gestor_entidad
ALTER column gestor_entidad_id SET DEFAULT nextval('sch_planificacion.seq_gestor_entidad');


ALTER TABLE sch_planificacion.tbl_meta_evidencia
alter column descripcion_evidencia type varchar(500);


ALTER TABLE sch_planificacion.tbl_reunion 
add column tipo_agendamiento_id  numeric;

ALTER TABLE sch_planificacion.tbl_resolucion ALTER COLUMN descripcion_responsable TYPE varchar(500) USING descripcion_responsable::varchar;
ALTER TABLE sch_planificacion.tbl_resolucion ALTER COLUMN nombre_archivo TYPE varchar(500) USING nombre_archivo::varchar;
ALTER TABLE sch_planificacion.tbl_resolucion ALTER COLUMN descripcion_resolucion TYPE varchar(500) USING descripcion_resolucion::varchar;

commit;

alter table sch_planificacion.tbl_ciclo_detalle_persona 
add column indicador_meta bpchar(1),
add column persona_evaluador_id numeric;

