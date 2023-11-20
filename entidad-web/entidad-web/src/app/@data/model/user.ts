export class User {
  // id: number;
  // idPersona: number;
  // nombre: string;
  // email: string;
  // login: string;
  // clave: string;
  // idEstado: number;
  // esAd: number;
  // fechaUltimaSesion: Date;
  numeroDocumento: number;
  token?: string;
  personaId?: number;
  nombres?: string;
  apellidoPaterno?: string;
  apellidoMaterno?: string;
  nombreCompleto?: string;
  direccion?: string;
  correo?: string;
  telefono?: string;
  idTelefono?: number;
  idCorreo?: number;
  anexo?: string;
  entidadId?: number;
  entidadNombre?: string;
  rolId?: number;
}

module.exports = User;
