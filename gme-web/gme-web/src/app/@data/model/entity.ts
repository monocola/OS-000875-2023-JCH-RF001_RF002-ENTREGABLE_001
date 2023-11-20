export class Entity {
  entidadId: number;
  sectorId: number;
  sector: string;
  nivelGobiernoId: number;
  nivelGobierno: string;
  descripcionEntidad: string;
  sigla: string;
  personaId: number;
  razonSocial: string;
  nombreComercial: string;
  tipoDocumento: string;
  numeroDocumento: string;
  direccionId: number;
  direccion: string;
  correoId: number;
  correo: string;
  telefonoId: number;
  telefono: string;
  anexo: string;
  logo: string;
  urlWeb: string;
  distritoId: number;
  distrito: string;
  provinciaId: number;
  provincia: string;
  departamentoId: number;
  departamento: string;
  flagActualiza: number;
  direccionFiscalId: number;
  direccionFiscal: string;
  urlPortada: string;
  nroSindicatos: number;
  tipoEntidadPub: string;
  tipoEntidadPubId: number;
}

export class ResumenServidoresCiviles {
  entidadId?: number;
  totalEntidad?: number;
  totalCarrerasEspeciales?: number;
  totalSindicalizados?: number;
  totalResponsablesOOUSUBOU?: number;
  totalModalidadFormativaSimilares?: number;
  totalEncargadosOOUSUBOU?: number;
}
