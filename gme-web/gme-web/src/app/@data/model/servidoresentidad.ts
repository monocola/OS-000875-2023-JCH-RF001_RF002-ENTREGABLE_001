
export class ServidorEntidad {
  idSolicitudEntidad: number;
  razonSocial: string;
  ruc: string;
  sectorId: number;
  sector: string;
  nivelGobiernoId: number;
  nivelGobierno: string;
  fechaRegistro: Date;
  tipoDocumentoId: string;
  descripcionTipoDocumento: string;
  numeroDocumento: string;
  nombres: string;
  apellidoPaterno: string;
  apellidoMaterno: string;
  correLaboral: string;
  correoOpcional: string;
  telefono: string;
  celular: string;
  celularAlterno: string;
  puestoId: number;
  descripcionPuesto: string;
  distritoId: number;
  distrito: string;
  provinciaId: number;
  provincia: string;
  departamentoId: number;
  departamento: string;
  estadoId: number;
  estadoSolicitud: string;
  fechaAlta: string;
  fechaBaja: string;
  cargoId: number;
  descripcionCargo: string;
  lugarDep: string;
  nombreCompleto: string;

  getLugarDep() {
    return `${this.departamento === null ? "-" : this.departamento}/${this.provincia === null ? "-" : this.provincia}/${this.distrito === null ? "-" : this.distrito}`;
  }

  getNombreCompleto() {
    return `${this.nombres === null ? "" : this.nombres} ${this.apellidoPaterno === null ? "" : " " + this.apellidoPaterno}/${this.apellidoMaterno === null ? "" : " " + this.apellidoMaterno}`;
  }

}


