export class Organo {
    estadoRegistro?: string;
    usuarioCreacion?: string;
    fechaCreacio?: string;
    usuarioModificacion?: string;
    fechaModificacion?: string;
    organigramaId: number;
    entidadId: number;
    areaId: number;
    sedeId: number;
    padreOrganigramaId?: number;
    puesto: string;
    orden: number;
    nivel: number;
    tipoOrganoUoId: number;
    naturalezaOrgano: number;
    nivelGobiernoId: number;
    descripcion: string;
    descripcionCorta?: string;
    sigla: string;
    personaResponsableId: string;
    telefono: string;
    telefonoId?: number;
    correo: string;
    correoId?: number;
    apellidoMaterno?: string;
    apellidoPaterno: string;
    desNaturaleza: string;
    desNivel: string;
    descripOrganoPadre: string;
    descripcionTipoOrg: string;
    estado?: string;
    nombrePais?: string;
    nombres: string;
    nroDocumento: string;
    tipoDocumento: number;
    settings: any;
    tipo?: number;
}