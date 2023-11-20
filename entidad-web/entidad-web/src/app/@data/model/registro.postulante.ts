export class RegistroPostulante {
  postulante: Postulante;

  constructor(postulante: Postulante) {
    this.postulante = postulante;
  }
}

class Postulante {
  usuarioId: number;
	personaId: number;
}
