export class ReniecResponse {
    apellidoMaterno: string;
    apellidoPaterno: string;
    estadoRegistro: string;
    fechaCreacion: string;
    nombreCompleto: string;
    nombres: string;
    personaId: number;
    usuarioCreacion: string;
    apellidoCasada?: string;
    estadoCivil?: string;
    fechaFallecimiento?: string;
    fechaModificacion?: string;
    fechaNacimiento?: string;
    paisId?: number;
    restriccionReniec?: string;
    sexo?: string;
    usuarioModificacion?: string;
    validadoMigracion?: string;
    validadoReniec?: string;
}

export class PaisReniec {
    codSunat: string;
    estadoRegistro: string;
    nombrePais: string;
    paisId: number;
}

export class PersonaNatural {
    apellidoMaterno: string;
    apellidoPaterno: string;
    estadoRegistro: string;
    fechaCreacion: string;
    nombreCompleto: string;
    nombres: string;
    personaId: number;
    usuarioCreacion: string;
    apellidoCasada?: string;
    estadoCivil?: string;
    fechaFallecimiento?: string;
    fechaModificacion?: string;
    fechaNacimiento?: string;
    paisId?: string;
    restriccionReniec?: string;
    sexo?: string;
    usuarioModificacion?: string;
    validadoMigracion?: string;
    validadoReniec?: string;
}
