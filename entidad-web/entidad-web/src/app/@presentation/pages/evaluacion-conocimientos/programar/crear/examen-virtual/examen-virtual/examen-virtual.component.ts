import { Component, OnInit, OnDestroy } from '@angular/core';
import { ActivatedRoute, Router } from '@angular/router';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EvaluacionConService } from '../../../../evaluacion-conocimientos.service';

const PUNTAJE_MAXIMO = 100;
const PUNTAJE_MINIMO = 70;
@Component({
  selector: 'serv-talento-examen-virtual',
  templateUrl: './examen-virtual.component.html',
  styleUrls: ['./examen-virtual.component.scss'],
})
export class ExamenVirtualComponent implements OnInit, OnDestroy {
  examenId = 0;
  pregunta = [];

  radioSelected: any;
  checkboxes = {};
  contador = 0;
  numeropregunta;
  cantidadpreguntas;
  descpregunta;
  respuestas;
  tipopregunta;
  tiempopregunta;
  interval;

  preguntaActual;
  puntosPorPregunta: any;
  cantPreguntasCorrectas;

  constructor(
    private toast: ToastService,
    private activatedRoute: ActivatedRoute,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    public helperService: EvaluacionConService,
    private router: Router
  ) {}
  ngOnInit(): void {
    if (!this.helperService.formProgramaciones)
      this.helperService.initializeForm();

    this.examenId = this.f.examenId.value;
    this.cargarPreguntas();
    this.cantPreguntasCorrectas = 0;
  }

  get f() {
    return this.helperService.formProgramaciones.controls;
  }

  obtenerPuntosPorPregunta(cantidadTotalpreguntas) {
    this.puntosPorPregunta = PUNTAJE_MAXIMO / cantidadTotalpreguntas;
  }

  preguntaAnterior() {
    this.contador--;
    this.numeropregunta = this.contador + 1;
    this.descpregunta = this.pregunta[this.contador].descripcion;
    this.tiempopregunta = this.pregunta[this.contador].duracion;
    this.respuestas = this.pregunta[this.contador].respuestas;
    this.tipopregunta = this.pregunta[this.contador].tipoPregunta.toUpperCase();
  }

  siguientePreguta() {
    this.preguntaActual = this.pregunta[this.contador];
    if (this.tipopregunta === 'CERRADA SIMPLE'.toUpperCase()) {
      /* if (this.radioSelected === undefined) {
        this.toast.showToast('Por favor marque una opción', 'warning');
        return;
      }*/
      this.verificarRespuestasSimple(
        this.preguntaActual.respuestas,
        this.radioSelected
      );
    }

    if (this.tipopregunta === 'CERRADA MÚLTIPLE'.toUpperCase()) {
      /* if (Object.keys(this.checkboxes).length === 0) {
        this.toast.showToast('Por favor marque almenos una opción', 'warning');
        return;
      }*/
      this.verificarRespuestasMultiple(
        this.preguntaActual.respuestas,
        this.checkboxes
      );
    }
    if (this.numeropregunta !== this.cantidadpreguntas) {
      this.contador++;
      this.numeropregunta = this.contador + 1;
      this.descpregunta = this.pregunta[this.contador].descripcion;
      this.tiempopregunta = this.pregunta[this.contador].duracion;
      this.respuestas = this.pregunta[this.contador].respuestas;
      this.tipopregunta = this.pregunta[
        this.contador
      ].tipoPregunta.toUpperCase();
    } else {
      let puntajeTotal = this.cantPreguntasCorrectas * this.puntosPorPregunta;
      let status = '';
      if (puntajeTotal >= PUNTAJE_MINIMO) {
        status = 'APROBADO';
      } else {
        status = 'DESAPROBADO';
      }

      this.helperService.enviarDatosResultadoExamen(
        status,
        puntajeTotal,
        PUNTAJE_MAXIMO,
        PUNTAJE_MINIMO
      );

      this.router.navigateByUrl(
        'pages/evaluacion-conocimientos/programar/grupos/examen-virtual/resultados'
      );
    }
  }

  private cargarPreguntas() {
    this.evaluacionConocimientosService
      .PreguntasByExamen(this.examenId)
      .subscribe((res) => {
        console.log(res);
        this.pregunta = res.listas;
        if (this.pregunta.length !== 0) {
          this.cargarPrimeraPregunta();
        }
      });
  }

  private cargarPrimeraPregunta() {
    this.numeropregunta = this.contador + 1;
    this.cantidadpreguntas = this.pregunta.length;
    this.descpregunta = this.pregunta[0].descripcion;
    this.respuestas = this.pregunta[0].respuestas;
    this.tipopregunta = this.pregunta[0].tipoPregunta.toUpperCase();
    this.tiempopregunta = this.pregunta[0].duracion;

    this.obtenerPuntosPorPregunta(this.cantidadpreguntas);
  }

  private obtenerRespuestaSeleccionada(checkboxSelected, respuestaId) {
    let id = null;
    for (let [key, value] of Object.entries(checkboxSelected)) {
      if (key === respuestaId && value === true) {
        id = respuestaId;
      }
    }
    return id;
  }

  private obtenerCantidadRespCorrectasMultiple(respuestas) {
    let totalCorrectos = 0;
    respuestas.forEach((element) => {
      if (element.esCorrecto === 'Correcto') {
        totalCorrectos++;
      }
    });
    return totalCorrectos;
  }

  private verificarRespuestasMultiple(respuestas, checkboxSelected) {
    let totalCorrectos = this.obtenerCantidadRespCorrectasMultiple(respuestas);
    let correctos = 0;
    let incorectos = 0;
    respuestas.forEach((element) => {
      let findResp = this.obtenerRespuestaSeleccionada(
        checkboxSelected,
        element.idAlternativa
      );
      if (findResp != null) {
        if (element.esCorrecto === 'Correcto') {
          correctos++;
        } else {
          incorectos++;
        }
      }
    });
    if (incorectos === 0 && totalCorrectos === correctos) {
      this.cantPreguntasCorrectas++;
    }
  }

  private verificarRespuestasSimple(respuestas, radioId) {
    respuestas.forEach((element) => {
      if (
        element.idAlternativa === radioId &&
        element.esCorrecto === 'Correcto'
      ) {
        this.cantPreguntasCorrectas++;
      }
    });
  }

  ngOnDestroy() {}
}
