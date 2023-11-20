export class Actividades {
  cronogramaId?: number;
  actividadId?: number;
  etapa?: string;
  actividades?: string;
  fechaIni?: string;
  fechaFin?: string;
  responsable?: string;
  numeracion?: number;

  settings?: any;
  flagEtapa?: any;
}

export class HistorialActividad {
  actividadHistorialId?: number;
  actividadId?: number;
  descripcionActividad?: string;
  descripcionEtapa?: string;
  descripcionResponsable?: string;
  etapaId?: number;
  fechaFin?: string;
  fechaInicio?: string;
  flagActividad?: string;
  flagDescripcionResponsable?: string;
  flagEtapa?: string;
  flagFechaFin?: string;
  flagFechaInicio?: string;
  settings?: number;
}
