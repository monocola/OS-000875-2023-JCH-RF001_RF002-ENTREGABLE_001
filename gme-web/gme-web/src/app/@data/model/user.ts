export class User {
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
