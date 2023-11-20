export class Cronograma {
  cronogramaId?: number;
  etapa?: string;
  actividades?: string;
  fechaInicio?: string;
  fechaFin?: string;
  responsables?: any[];
  numeracion?: number;
  actividadId?: number;
  descripcion?: string;
  descripcionActividad?: string;
  etapaId?: number;
  responsable?: string;
  descripcionEtapa?: string;
  listResponsable?: string;
}

export class Responsable {
  responsableId: number;
  descripcionResponsable: string;
}
