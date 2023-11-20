export class RegistroEntidad {

  constructor(nroRuc?: string, razonSocial?: string, nombre?: string, sector?: string, nivelGobierno?: string, siglas?: string, direccion?: string, idDepartamento?: number, idProvincia?: number, idDistrito?: number, telefono?: string, anexo?: string, correomesapartes?: string) {
    this.nroRuc = nroRuc;
    this.razonSocial = razonSocial;
    this.nombre = nombre;
    this.sector = sector;
    this.nivelGobierno = nivelGobierno;
    this.siglas = siglas;
    this.direccion = direccion;
    this.idDepartamento = idDepartamento;
    this.idProvincia = idProvincia;
    this.idDistrito = idDistrito;
    this.telefono = telefono;
    this.anexo = anexo;
    this.correomesapartes = correomesapartes;
  }

  nroRuc: string = "";
  razonSocial: string = "";
  nombre: string = "";
  sector: string = "";
  nivelGobierno: string = "";
  siglas: string = "";
  direccion: string = "";
  idDepartamento: number = null;
  idProvincia: number = null;
  idDistrito: number = null;
  telefono: string = "";
  anexo: string = "";
  correomesapartes: string = "";
}
