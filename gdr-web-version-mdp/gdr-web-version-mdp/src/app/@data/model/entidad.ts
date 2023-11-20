export class ApiEntidad {
  entidadId?: number;
  sectorId?: number;
  sector?: string;
  nivelGobiernoId?: number;
  nivelGobierno?: string;
  descripcionEntidad?: string;
  siglaDescripcion?: string;
  sigla?: string;
  personaId?: number;
  razonSocial?: string;
  nombreComercial?: string;
  tipoDocumento?: number;
  numeroDocumento?: string;
  direccionId?: number;
  direccion?: string;
  correoId?: number;
  correo?: string;
  telefonoId?: number;
  telefono?: string;
  anexo?: string;
  logo?: string;
  urlWeb?: string;
  distritoId?: number;
  distrito?: string;
  provinciaId?: number;
  provincia?: string;
  departamentoId?: number;
  departamento?: string;
  flagActualiza?: string;
  direccionFiscalId?: number;
  direccionFiscal?: string;
  urlPortada?: string;
}

export class ApiPlaniEntidad {
  entidadId?: number;
  entidad?: string;
  cicloIdId?: number;
  anioCiclo?: string;
  nivelGobiernoId?: number;
  nivelGobierno?: string;
  sectorId?: number;
  sector?: string;
  tipoEntidadId?: number;
  tipoEntidad?: string;
  conteoGestorGDR?: number;
  estadoId?: number;
  estado?: string;
  contGestor?: string;
  colorEstado?: string;
}
