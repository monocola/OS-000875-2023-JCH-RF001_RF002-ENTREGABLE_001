export interface Reuniones {
   reunionId?: number;
   nombreEvaluado?: string;
   evaluadoDetalleUoId?: number;
   evaluadoPersonaId?: string;
   descripcionPuesto?: string;
   puesto?: string;
   fechaReunion?: Date | string;
   horaReunion?: string;
   duracion?: number;
   cicloDetallePersonaId?: string;
   cicloId?: number;
   detaUoId?: number;
   tipoAgendamientoId?: number;
   enviaraNotificacion?: number;
   esEvaluado?: boolean;
}


export interface TabsPuestos {
   descripcion ?: string;
   sigla ?: string;
   detalleUoId ?: number;
   organigramaId ?: number;
   entidadId ?: number;
   personaId?: number;
   puestoId?: number;
   responsable?: number;
   rolId?: number;
   evaluadoDetalleUoId?: number;
   detUnidadOrganicaId?: number;
}

export interface HistorialReu {
   descripcionReunion?: string;
   duracion?: number;
   estadoRegistro?: string;
   estadoReunion?: string;
   evaluadoDetalleUoId?: number;
   evaluadoPersonaId?: number;
   fechaCreacion?: Date | string;
   fechaModificacion?: string;
   fechaReunion?: Date | string;
   horaReunion?: string;
   reunionId?: number;
   usuarioCreacion?: string;
   usuarioModificacion?: string;
}
