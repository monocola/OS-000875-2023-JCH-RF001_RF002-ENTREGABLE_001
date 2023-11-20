import { Injectable } from '@angular/core';
import { Adapter } from './adapter';

export class EntityRequest {
  constructor(
    public solicitudEntidad: Solicitud,
    public personaNatural: Person,
    public personaJuridica: Person,
    public archivoSolicitud: Adjunto
  ) {}
}

@Injectable({
  providedIn: 'root',
})
export class EntityAdapter implements Adapter<EntityRequest> {
  adapt(response: any): EntityRequest {
    return new EntityRequest(
      Object.assign(new Solicitud(), response.solicitudEntidad),
      Object.assign(new Person(), response.listaSolicitudPersona[1]),
      Object.assign(new Person(), response.listaSolicitudPersona[0]),
      Object.assign(new Adjunto(), response.listaSolicitudArchivo[0])
    );
  }
}

export class Solicitud {
  solicitudEntidadId: number;
  sector: number;
  nivelGobierno: number;
  tipo: number;
  tipoEntidad: number;
  aceptoTerminosCondiciones: string;
  numeroIntentos: number;
  aplicacionId: number;
  estadoSolicitud: number;
  descripcionNivelGobierno: string;
  descripcionSector: string;
  fechaCreacion: string;
  fechaModificacion: string;
  usuarioCreacion: string;
  usuarioModificacion: string;
  estadoRegistro: string;
}

export class Adjunto {
  solicitudArchivoId: number;
  nombreArchivo: string;
  nombreRealArchivo: string;
  tipoArchivo: number;
  archivo: string;
  estado: string;

  public getExtension() {
    return this.nombreRealArchivo.split('.')[
      this.nombreRealArchivo.split('.').length - 1
    ];
  }
}

export class Person {
  actividadEconomicaPrincipal: string;
  apellidoCasada: string;
  apellidoMaterno: string;
  apellidoPaterno: string;
  cargoId: number;
  celularLaboral: string;
  celularPrincipal: string;
  celularSecundario: string;
  condicionContribuyente: string;
  correoId: number;
  correoLaboral: string;
  correoPrincipal: string;
  correoSecundario: string;
  cuentaClienteId: string;
  descripcionCargo: string;
  descripcionPais: string;
  descripcionTipoDoc: string;
  direccionCompleta: string;
  estadoCivil: number;
  estadoConstribuyente: string;
  fechaInicioActividades: string;
  fechaInscripcion: string;
  fechaNacimiento: string | Date;
  imagen: string;
  lugarNacimiento: string;
  nombreComercial: string;
  nombres: string;
  numeroDocumento: string;
  paisId: number;
  personaId: number;
  puestoTrabajoId: number;
  razonSocial: string;
  referenciaDireccion: string;
  rolEntidadId: number;
  rutaPaginaWeb: string;
  sexo: string;
  sigla: string;
  solicitudEntidadId: number;
  solicitudPersonaId: number;
  telefonoFijo: string;
  telefonoId: number;
  tipoDocumento: number;
  tipoPersona: number;
  ubigeoId: number;
  validar: string;
  listaArchivo: any;

  public getFullNameAdmin() {
    return `${this.nombres} ${this.apellidoPaterno} ${
      this.apellidoMaterno || ''
    }`.trim();
  }

  public getSexo() {
    return this.sexo === '1' ? 'MASCULINO' : 'FEMENINO';
  }
}
