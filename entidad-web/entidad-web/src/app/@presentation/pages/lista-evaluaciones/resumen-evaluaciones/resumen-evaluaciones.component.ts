import { Component, OnInit } from '@angular/core';
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';
import { CapturaImagenResumen } from 'src/app/@data/model/examenCorregido';
import { ResumenRespuestas } from 'src/app/@data/model/lista-evaluaciones/entity';
import { FormArray, FormBuilder, FormGroup } from '@angular/forms';
import { ToastService } from 'src/app/@presentation/@common-components/toast';

@Component({
  selector: 'serv-talento-resumen-evaluaciones',
  templateUrl: './resumen-evaluaciones.component.html',
  styleUrls: ['./resumen-evaluaciones.component.scss'],
})
export class ResumenEvaluacionesComponent implements OnInit {
  puesto: string = '';
  listCapturaImagenResumen: CapturaImagenResumen[];

  preguntasCorrectas: string[];
  preguntasIncorrectas: string[];
  preguntasAbiertasEvauadas: string[];
  ptsTotPregCerrada: number;
  ptsObtPregCerrada: number;
  ptsTotPregAbierta: number;
  ptsObtPregAbierta: number;

  cantidadPreguntas: string;
  tiempoExamen: string;
  lstPreguntasCerradas: any[];
  lstPreguntasAbiertas: any[];
  puntajeTotal: number;
  situacionExamen: string;
  fechaEvaluacion: string;

  isDataReady: boolean;
  localPostulante: any;
  localConvocatoriaId: string;
  objConvocatoria: any;
  codigoConvocatoria: string;
  preguntasForm: FormGroup;
  listResumenRespuestas: ResumenRespuestas;

  constructor(
    private srvSeleccionRepository: SeleccionServirRepository,
    private fb: FormBuilder,
    private toast: ToastService
  ) {
    this.localPostulante = JSON.parse(
      localStorage.getItem('datosPostulanteResumen')
    );
    this.localConvocatoriaId = sessionStorage.getItem('convocatoriaId');
    this.preguntasForm = this.fb.group({
      preguntas: new FormArray([]),
    });
  }

  get f() {
    return this.preguntasForm.controls;
  }

  get p() {
    return this.f.preguntas as FormArray;
  }

  get preguntasFormGroup() {
    return this.p.controls as FormGroup[];
  }

  ngOnInit(): void {
    this.listarResumenExamen();
  }

  listarResumenExamen() {
    this.srvSeleccionRepository
      .listarExamenCorregido(this.localPostulante.idCabecera)
      .subscribe((examen) => {
        this.puesto = this.localPostulante.desPerfil;
        this.cantidadPreguntas = examen.cantidadpreguntas;
        this.tiempoExamen = examen.tiempoExamen;
        this.puntajeTotal = examen.puntajeTotal;
        this.situacionExamen = examen.situacionExamen;
        this.fechaEvaluacion = examen.fechaEvaluacion;
        this.lstPreguntasCerradas = examen.lstPreguntas.filter((item) => {
          return item.tipoPregunta === 1 || item.tipoPregunta === 2;
        });
        this.lstPreguntasAbiertas = examen.lstPreguntas.filter((item) => {
          return item.tipoPregunta === 3;
        });

        this.lstPreguntasAbiertas.forEach((item) => {
          this.p.push(
            this.fb.group({
              preguntaId: [item.preguntaId],
              orden: [item.orden],
              descripcion: [item.descripcion],
              tipoPregunta: [item.tipoPregunta],
              respuestaAbierta: [item.respuestaAbierta],
              incremental: [item.incremental],
              puntaje: [item.respuestaPuntaje],
            })
          );
        });

        this.getDetalleExamen();
        this.listarDatosConvocatoria();
        this.listarCarrusel();
      });
  }

  guardarPuntaje() {
    let lstPreguntasForm: any[] = this.p.value;
    lstPreguntasForm.forEach(it => {
      it.baseId = this.localPostulante.baseId;
    });
    this.srvSeleccionRepository
      .guardarPregAbiertas(this.localPostulante.idCabecera, lstPreguntasForm)
      .subscribe((response) => {
        this.toast.showToast(response, 'success');
        this.CargaDatos();
      });

  }

  CargaDatos() {
    this.srvSeleccionRepository
      .listarExamenCorregido(this.localPostulante.idCabecera)
      .subscribe((examen) => {
        this.puesto = this.localPostulante.desPerfil;
        this.cantidadPreguntas = examen.cantidadpreguntas;
        this.tiempoExamen = examen.tiempoExamen;
        this.puntajeTotal = examen.puntajeTotal;
        this.situacionExamen = examen.situacionExamen;
        this.fechaEvaluacion = examen.fechaEvaluacion;
        this.lstPreguntasCerradas = examen.lstPreguntas.filter((item) => {
          return item.tipoPregunta === 1 || item.tipoPregunta === 2;
        });
        this.lstPreguntasAbiertas = examen.lstPreguntas.filter((item) => {
          return item.tipoPregunta === 3;
        });

        this.getDetalleExamen();
        this.listarDatosConvocatoria();
        this.listarCarrusel();
      });
  }


  getDetalleExamen() {
    this.srvSeleccionRepository
      .getDetalleExamen(this.localPostulante.idCabecera)
      .subscribe((mc) => {
        this.listResumenRespuestas = mc;
        this.preguntasCorrectas = this.listResumenRespuestas.preguntasCorrectas;
        this.preguntasIncorrectas = this.listResumenRespuestas.preguntasIncorrectas;
        this.preguntasAbiertasEvauadas = this.listResumenRespuestas.preguntasAbiertas;
        this.ptsTotPregCerrada = this.listResumenRespuestas.ptsTotPregCerrada;
        this.ptsObtPregCerrada = this.listResumenRespuestas.ptsObtPregCerrada;
        this.ptsTotPregAbierta = this.listResumenRespuestas.ptsTotPregAbierta;
        this.ptsObtPregAbierta = this.listResumenRespuestas.ptsObtPregAbierta;
        this.puntajeTotal = this.listResumenRespuestas.puntajeTotal;
      });
  }

  listarCarrusel() {
    this.srvSeleccionRepository
      .getCapturaImagenPostulante(
        this.localPostulante.idExamen,
        this.localPostulante.idPostulante
      )
      .subscribe((mc) => {
        this.listCapturaImagenResumen = mc.items;
        this.isDataReady = true;
      });
  }

  listarDatosConvocatoria() {
    this.srvSeleccionRepository
      .getDatosConvocatoriaById(Number(this.localConvocatoriaId))
      .subscribe((mc) => {
        this.objConvocatoria = mc;
        this.codigoConvocatoria = mc.codigoConvocatoria;
      });
  }
}
