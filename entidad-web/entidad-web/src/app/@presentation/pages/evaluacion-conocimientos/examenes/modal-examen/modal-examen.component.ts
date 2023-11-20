import { Component, Inject, Input, OnDestroy, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Const } from 'src/app/@data/services/const';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import forEach = CKEDITOR.tools.array.forEach;

@Component({
  selector: 'serv-modal-examen',
  templateUrl: './modal-examen.component.html',
  styleUrls: ['./modal-examen.component.scss'],
})
export class ModalCrearExamenComponent implements OnInit, OnDestroy {
  const = Const;

  preguntasColumns = [];
  categorias = [];
  isCategoriaSelected: boolean = false;
  preguntas = [];
  show: boolean = true;
  cantidadPreguntas = 0;
  idExamen: any;
  isEditar: any;
  titulo: any;
  payloadPreguntas = [];
  payloadPreguntasSave = [];
  idCategoria = null;
  descripcionCombo = '';
  sumatoriaPuntos = 0;

  @Input() puntaje;

  registerForm: FormGroup = this.fb.group({
    categoria: [null, Validators.required],
  });

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalCrearExamenComponent>,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private toast: ToastService,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  async ngOnInit() {
    this.dialogRef.updateSize('55%', '90%');
    this.titulo = this.data.titulo;
    this.idExamen = this.data.idExamen;
    this.isEditar = this.data.isEditar;
    this.idCategoria = this.data.idCategoria;
    this.initializeColumns();
    this.getBandejaCategoria();
  }

  initializeColumns() {
    this.preguntasColumns = [
      {
        name: 'Marcar',
        dataKey: 'marcar',
        position: 'left',
        isSortable: true,
        width: '8%',
        descripcion: 'marcar',
        flagSeleccion: 'flagSeleccion',
        preguntaId: 'preguntaId',
      },
      {
        name: 'Descripcion',
        dataKey: 'descripcion',
        position: 'left',
        isSortable: true,
        width: '70%',
        descripcion: 'descripcion',
        flagSeleccion: 'flagSeleccion',
        preguntaId: 'preguntaId',
      },
     {
       name: 'Puntaje',
       dataKey: 'puntaje',
       position: 'left',
       isSortable: true,
       width: '10%',
       descripcion: 'puntaje',
       flagSeleccion: 'flagSeleccion',
       preguntaId: 'preguntaId',
     },
    ];
  }

  getBandejaCategoria() {
    this.evaluacionConocimientosService
      .buscarBandejaCategoria(null, null)
      .subscribe((res) => {
        this.categorias = [...res];

        if (this.categorias.length > 0) {
          if (this.idCategoria != null) {
            this.f.categoria.setValue(this.idCategoria);

            for (let index = 0; index < this.categorias.length; index++) {
              const element = this.categorias[index];

              if (element.categoriaId === this.idCategoria) {
                this.descripcionCombo = element.descripcion;
              }
            }

            this.changeCategoria();
          }
        }
      });
  }

  changeCategoria() {
    if (this.f.categoria.value) {
      this.evaluacionConocimientosService
        .listarPreguntasPorCategoria(this.f.categoria.value, this.idExamen)
        .subscribe((res) => {
          this.preguntas = [...res.preguntas];
          this.cantidadPreguntas = this.preguntas.length;
          this.isCategoriaSelected = true;
          this.preguntas.forEach((element) => {
            if (element.flagSeleccion) {
              this.payloadPreguntas.push({
                examenId: this.idExamen,
                preguntaId: element.preguntaId,
                puntajePregunta: element.puntajePregunta,
                flagSeleccion: 1,
              });
            }

            this.payloadPreguntasSave.push({
              examenId: this.idExamen,
              preguntaId: element.preguntaId,
              puntajePregunta: element.puntajePregunta,
              flagSeleccion: element.flagSeleccion ? 1 : 0,
            });

          });

          this.sumarPuntos();
        });
    } else {
      this.isCategoriaSelected = false;
    }
  }

  onCheckItem(event) {
    let checked = event.flagSeleccion;
    let idPregunta = event.preguntaId;
    let puntajePregunta = Number(event.puntajePregunta);

    if (checked) {
      this.payloadPreguntas.push({
        examenId: this.idExamen,
        preguntaId: idPregunta,
        flagSeleccion: 1,
        puntajePregunta: isNaN(puntajePregunta) ? 0 : puntajePregunta
      });

      for (let index = 0; index < this.payloadPreguntasSave.length; index++) {
        const element = this.payloadPreguntasSave[index];
        if (element.preguntaId === idPregunta) {
          this.payloadPreguntasSave[index].flagSeleccion = 1;
          this.payloadPreguntasSave[index].puntajePregunta = isNaN(puntajePregunta) ? 0 : puntajePregunta;
        }
      }
    } else {
      for (let index = 0; index < this.payloadPreguntas.length; index++) {
        const element = this.payloadPreguntas[index];
        if (element.preguntaId === idPregunta) {
          this.payloadPreguntas.splice(index, 1);
        }
      }

      for (let index = 0; index < this.payloadPreguntasSave.length; index++) {
        const element = this.payloadPreguntasSave[index];
        if (element.preguntaId === idPregunta) {
          this.payloadPreguntasSave[index].flagSeleccion = 0;
          this.payloadPreguntasSave[index].puntajePregunta = isNaN(puntajePregunta) ? 0 : puntajePregunta;
        }
      }
    }

    this.sumarPuntos();
  }

  onInputItemOut (event) {

    // -- SECCIÓN payloadPreguntas
    let index = this.payloadPreguntas.indexOf(this.payloadPreguntas.find((item) => item.preguntaId === event.preguntaId));
    if (index < 0 && event.flagSeleccion) {
      // Regstraro un nuievo objeto en payload
      this.payloadPreguntas.push({
        examenId: this.idExamen,
        preguntaId: event.preguntaId,
        flagSeleccion: event.flagSeleccion ? 1 : 0,
        puntajePregunta: isNaN(parseFloat(event.puntajePregunta)) ? 0 : parseFloat(event.puntajePregunta)
      });
    } else if ( index !== -1 ) {
      // Sino se actualiza
      this.payloadPreguntas[index] = {
        examenId: this.idExamen,
        preguntaId: event.preguntaId,
        flagSeleccion: event.flagSeleccion ? 1 : 0,
        puntajePregunta: isNaN(parseFloat(event.puntajePregunta)) ? 0 : parseFloat(event.puntajePregunta)
      };
    }

    // -SECCCIÓN payloadPreguntasSave
    let indexSave = this.payloadPreguntasSave.indexOf(this.payloadPreguntasSave.find((item) => item.preguntaId === event.preguntaId));
    if (indexSave < 0 && event.flagSeleccion) {
      this.payloadPreguntasSave.push({
        examenId: this.idExamen,
        preguntaId: event.preguntaId,
        flagSeleccion: event.flagSeleccion ? 1 : 0,
        puntajePregunta: isNaN(parseFloat(event.puntajePregunta)) ? 0 : parseFloat(event.puntajePregunta)
      });
    } else if ( indexSave !== -1 ) {
      this.payloadPreguntasSave[indexSave] = {
        examenId: this.idExamen,
        preguntaId: event.preguntaId,
        flagSeleccion: event.flagSeleccion ? 1 : 0,
        puntajePregunta: isNaN(parseFloat(event.puntajePregunta)) ? 0 : parseFloat(event.puntajePregunta)
      };
    }

    this.sumarPuntos();
  }

  sumarPuntos(): void {
    this.sumatoriaPuntos = 0;

    if (this.isEditar) {
      for (let index = 0; index < this.payloadPreguntasSave.length; index++) {
        if (this.payloadPreguntasSave[index].flagSeleccion === 1) {
          let punto = this.payloadPreguntasSave[index].puntajePregunta === null ? 0 : this.payloadPreguntasSave[index].puntajePregunta;
          this.sumatoriaPuntos += punto;
        }
      }
    } else {
      for (let index = 0; index < this.payloadPreguntas.length; index++) {
        if (this.payloadPreguntas[index].flagSeleccion === 1) {
          let punto = this.payloadPreguntas[index].puntajePregunta === null ? 0 : this.payloadPreguntas[index].puntajePregunta;
          this.sumatoriaPuntos += punto;
        }
      }
    }
  }

  onClickGuardar() {
    if (this.validarPuntajes()) {

        let request = [];
        if (this.registerForm.valid) {
          if (this.payloadPreguntas.length > 0) {
            if (this.isEditar) {
              request = this.payloadPreguntasSave;
            } else {
              request = this.payloadPreguntas;
            }

            this.evaluacionConocimientosService
              .guardarActualizarDetalleExamen(request)
              .subscribe((res) => {
                if (res.status.success) {
                  this.toast.showToast(res.payload.mensaje, 'success', 'Atención');
                  this.onNoClick(true);
                } else {
                  this.toast.showToast(
                    res.status.error.messages[0],
                    'danger',
                    'Atención'
                  );
                }
              });
          } else {
            this.toast.showToast(
              'Debe seleccionar al menos una pregunta',
              'danger'
            );
          }
        } else {
          this.toast.showToast('Debe completar los campos obligatorios', 'danger');
        }
    } else {
      this.toast.showToast('Debe completar los puntajes obligatorios', 'warning');
    }
  }

  validarPuntajes(): boolean {
    let isPassed = true;
    this.payloadPreguntas.forEach(function ( item ) {
      if (!Number(item.puntajePregunta)
        || item.puntajePregunta === undefined
        || item.puntajePregunta === ""
        || item.puntajePregunta === "0") {
        isPassed = false;
        return isPassed;
      }
    });
    return isPassed;
  }

  ngOnDestroy(): void {
    // this.editMode = false;
  }

  get f() {
    return this.registerForm.controls;
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }
}
