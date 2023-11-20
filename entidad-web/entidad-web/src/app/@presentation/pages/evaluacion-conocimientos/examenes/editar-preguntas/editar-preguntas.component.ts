import { getBase64 } from 'src/app/utils/converterFile';
import { forkJoin } from 'rxjs';
import { MaestraService } from 'src/app/@data/services/maestra.service';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { EvaluacionConocimientosRepository } from 'src/app/@domain/repository/evaluacion-conocimientos.repository';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ModalCrearPreguntaComponent } from './../../preguntas/modal-pregunta.component';
import { Validators, FormGroup, FormBuilder } from '@angular/forms';
import { Component, OnInit, Inject, HostListener, OnDestroy } from '@angular/core';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-editar-preguntas',
  templateUrl: './editar-preguntas.component.html',
  styleUrls: ['./editar-preguntas.component.scss']
})
export class EditarPreguntasComponent implements OnInit, OnDestroy {
  const = Const;

  cmbDuracion = [];
  cmbTipoPregunta: any = [];
  show: boolean = true;
  showCorrect: boolean = false;
  cantidadCorrectas = 0;
  preguntaData: any;
  idPregunta = null;
  idCategoria = null;
  categoria = null;
  alternativas = [];
  mensaje: any;
  isVerDetalle = null;
  titulo: any;
  fileName = '';
  disabledValue: 'disabled';
  cantRespuestasPermitidas = 0;
  validador: boolean = false;
  viewAlternative: boolean = false;
  viewIncremento: boolean = false;

  registerForm: FormGroup = this.fb.group({
    descripcion: ['', Validators.required],
    categoria: [''],
    duracion: [null],
    tipoPregunta: [null, Validators.required],
    puntos: [null, Validators.required],
    rangoIncremental: [null],
    expopc: '',
  });

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalCrearPreguntaComponent>,
    private evaluacionConocimientosService: EvaluacionConocimientosRepository,
    private toast: ToastService,
    private maestraService: MaestraService,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) {}

  async ngOnInit() {
    this.dialogRef.updateSize('50%', '80%');
    this.fileName = null;
    this.idCategoria = this.data.idCategoria;
    this.f.categoria.setValue(this.data.categoria);
    this.idPregunta = this.data.idPregunta;
    this.isVerDetalle = this.data.isVerDetalle;
    this.titulo = this.data.titulo;

    if (this.idPregunta != null) {
      this.getListaDuracionPregunta();
    } else {
      this.fileName = 'Subir imagen de la pregunta';
      forkJoin([
        this.evaluacionConocimientosService.listarTipoDuraciones(),
      ]).subscribe((res) => {
        this.cmbDuracion = res[0];
      });
    }

    this.maestraService
      .getMaestraDetalleByCod('TIP_PREGUNTA')
      .subscribe((res) => {
        this.cmbTipoPregunta = res;
      });
  }

  @HostListener('document:keydown.tab', ['$event'])
  onKeydownHandler(event: KeyboardEvent) {
    if (this.isVerDetalle) {
      event.preventDefault();
    }
  }

  getDetallePregunta(result) {
    let res = result[0];

    this.idPregunta = res.idPregunta;
    this.f.duracion.setValue(res.idDuracion);
    this.idCategoria = res.idCategoria;
    this.f.descripcion.setValue(res.descPregunta);
    this.f.expopc.setValue(res.explicacionOpc);
    this.f.puntos.setValue(res.puntos);
    this.f.rangoIncremental.setValue(res.rangoIncremental);
    this.f.tipoPregunta.setValue('' + res.tipoPregunta);

    if (this.registerForm.get('tipoPregunta').value === '3') {
      this.viewIncremento = true;
    }

    if (res.imagenUrl === null) {
      if (!this.isVerDetalle) {
        this.fileName = 'Subir imagen de la pregunta';
      }
    } else {
      this.fileName = res.imagenUrl;
    }

    if (res.alternativas != null && res.alternativas.length > 0) {
      res.alternativas.forEach((element) => {
        let alternativaObj = {
          idAlternativa: null,
          icono: null,
          descripcion: null,
          css: null,
          esCorrecto: null,
        };
        alternativaObj.idAlternativa = element.idAlternativa;
        alternativaObj.descripcion = element.descripcion;
        alternativaObj.esCorrecto = element.esCorrecto;

        if (element.esCorrecto) {
          alternativaObj.icono = 'ent-web-check';
          alternativaObj.css = 'btn-card-green';
        } else {
          alternativaObj.icono = 'ent-web-clean';
          alternativaObj.css = 'btn-card-red';
        }
        this.alternativas.push(alternativaObj);
      });

      this.show = false;
      this.showCorrect = true;
    }
  }

  getListaDuracionPregunta() {
    this.evaluacionConocimientosService
      .listarTipoDuraciones()
      .subscribe((res) => {
        this.cmbDuracion = res;

        forkJoin([
          this.evaluacionConocimientosService.obtenerDetallePregunta(
            this.idPregunta
          ),
        ]).subscribe((response) => {
          this.getDetallePregunta(response);
        });
      });
  }

  changeDuracion() {}

  onClickGuardar() {
    if (this.registerForm.valid) {
      let validar = this.validarDataAlternativas();
      if (validar.flag) {
        this.toast.showToast(validar.mensaje, 'danger', 'Atención');
      } else {
        if (this.fileName === 'Subir imagen de la pregunta') {
          this.fileName = null;
        }
        if (this.f.descripcion.value.length > 500) {
          this.toast.showToast(
            'La descripción de la pregunta no debe exceder a 500 carácteres.',
            'danger'
          );
          this.validador = false;
        } else {
          this.validador = true;
        }
        if (this.f.expopc.value.length > 500) {
          this.toast.showToast(
            'La explicación opcional no debe exceder a 500 carácteres.',
            'danger'
          );
          this.validador = false;
        } else {
          this.validador = true;
        }

        for (let i = 0; i < this.alternativas.length; i++) {
          if (this.alternativas[i].descripcion.length > 500) {
            this.toast.showToast(
              'La alternativa ' +
                (i + 1) +
                ' no debe exceder a 500 carácteres.',
              'danger'
            );
            this.validador = false;
          }
        }

        if (
          this.validador &&
          this.f.descripcion.value.length < 500 &&
          this.f.expopc.value.length < 500
        ) {
          this.evaluacionConocimientosService
            .guardarActualizarPregunta(
              this.idPregunta,
              this.f.duracion.value,
              this.idCategoria,
              this.f.descripcion.value,
              this.fileName,
              this.f.tipoPregunta.value,
              this.f.expopc.value,
              this.alternativas,
              this.f.puntos.value,
              this.f.rangoIncremental.value
            )
            .subscribe((res) => {
              if (res.status.success) {
                this.toast.showToast(
                  res.payload.mensaje,
                  'success',
                  'Atención'
                );

                this.onNoClick(true);
              } else {
                this.toast.showToast(
                  res.status.error.messages[0],
                  'danger',
                  'Atención'
                );
              }
            });
        }
      }
    } else {
      this.toast.showToast('Debe completar los campos obligatorios', 'danger');
    }
  }

  onFileSelected(event: any) {
    const file: File = event.target.files[0];

    if (file) {
      const extension = file.name.split('.')[file.name.split('.').length - 1];

      if (extension.toUpperCase() === 'PNG') {
        const reader = new FileReader();
        reader.readAsDataURL(file);
        reader.onload = () => {
          const img = new Image();
          img.src = reader.result as string;
          img.onload = () => {
            const height = img.naturalHeight;
            const width = img.naturalWidth;

            if (!(height <= 512 && width <= 512)) {
              this.toast.showToast(
                'La imagen debe tener un tamaño máximo de 512 x 512',
                'danger'
              );
            } else {
              const reqFileName = file.name.split('.')[0];
              const pattern = new RegExp(/^[A-Za-z0-9]+$/g);

              if (!pattern.test(reqFileName)) {
                this.toast.showToast(
                  ' El nombre de la imagen solo debe contener números y letras.',
                  'danger',
                  'Atención'
                );
              } else {
                this.fileName = file.name;

                getBase64(file).then((data: string) => {
                  let imgData64 = data.split(',')[1];

                  this.evaluacionConocimientosService
                    .fileUpload(
                      imgData64,
                      reqFileName,
                      '.' + extension,
                      '',
                      'evaluacionconocimiento/pregunta'
                    )
                    .subscribe((res) => {
                      if (res.status.success) {
                        this.toast.showToast(
                          'Imagen registrada correctamente',
                          'success',
                          'Atención'
                        );
                      } else {
                        this.toast.showToast(
                          res.status.error.messages[0],
                          'danger',
                          'Atención'
                        );
                      }
                    });
                });
              }
            }
          };
        };
      } else {
        this.toast.showToast(
          'Sólo se admiten imagenes del tipo .png',
          'danger'
        );
      }
    }
  }

  validarDataAlternativas() {
    let countCorrectas = 0;
    let countIncorrectas = 0;
    let response = { flag: false, mensaje: 'sdsdsasad' };
    let tipo = +this.registerForm.get('tipoPregunta').value;

    if (
      this.f.tipoPregunta.value === '1' ||
      this.f.tipoPregunta.value === '2'
    ) {
      if (this.alternativas.length <= 1) {
        response.flag = true;
        response.mensaje = 'Debe ingresar al menos dos alternativas';
      } else {
        for (let i = 0; i < this.alternativas.length; i++) {
          let item = this.alternativas[i];

          if (
            item.descripcion === null ||
            (item.descripcion != null && item.descripcion.trim() === '')
          ) {
            response.flag = true;
            response.mensaje =
              'No se puede guardar alternativas con campos vacíos';
            break;
          } else {
            if (item.esCorrecto) {
              countCorrectas++;
            } else {
              countIncorrectas++;
            }
          }
        }

        if (!response.flag) {
          if (countCorrectas === 0 || countIncorrectas === 0) {
            response.flag = true;
            response.mensaje =
              'Debe regitrar al menos una pregunta correcta y una incorrecta';
          } else if (tipo === 1 && countCorrectas > 1) {
            response.flag = true;
            response.mensaje =
              'Las preguntas simples solo deben contener una respuesta correcta';
          } else if (tipo === 2 && countCorrectas === 1) {
            response.flag = true;
            response.mensaje =
              'Las preguntas multiples deben contener mas de una respuesta correcta';
          }
        }
      }
    } else if (+this.f.rangoIncremental.value > this.f.puntos.value) {
      response.flag = true;
      response.mensaje =
        'El rango incremental no puede ser mayor al puntaje de la pregunta';
    } else if (tipo === 3 && !this.getDivisible) {
      response.flag = true;
      response.mensaje =
        'El puntaje debe ser divisible por el rango incremental';
    }
    return response;
  }

  get getDivisible() {
    let div1 = this.f.puntos.value;
    let div2 = +this.f.rangoIncremental.value;

    if ((div1 / div2) % 1 === 0) {
      return true;
    } else {
      return false;
    }
  }

  onClickSwapTypeAlternativa(i) {
    this.alternativas[i].esCorrecto = !this.alternativas[i].esCorrecto;
    if (this.alternativas[i].esCorrecto) {
      this.alternativas[i].icono = 'ent-web-check';
      this.alternativas[i].css = 'btn-card-green';
    } else {
      this.alternativas[i].icono = 'ent-web-clean';
      this.alternativas[i].css = 'btn-card-red';
    }
  }

  onClickRemoveAlternativa(i) {
    this.alternativas.splice(i, 1);
    if (this.alternativas.length <= 0) {
      this.show = true;
    }
  }

  onClickAddCorrecto() {
    this.evaluacionConocimientosService
      .maxCantidadRespuestas()
      .subscribe((res) => {
        this.cantRespuestasPermitidas = res;
        if (this.alternativas.length < this.cantRespuestasPermitidas) {
          this.show = false;
          this.showCorrect = true;
          this.alternativas.push({
            idAlternativa: null,
            icono: 'ent-web-check',
            descripcion: null,
            css: 'btn-card-green',
            esCorrecto: true,
          });
        } else {
          this.toast.showToast(
            'Sólo está permitido un máximo de ' +
              this.cantRespuestasPermitidas +
              ' respuestas',
            'danger',
            'Atención'
          );
        }
      });
  }

  onClickAddIncorrecto() {
    this.evaluacionConocimientosService
      .maxCantidadRespuestas()
      .subscribe((res) => {
        this.cantRespuestasPermitidas = res;
        if (this.alternativas.length < this.cantRespuestasPermitidas) {
          this.show = false;
          this.showCorrect = true;
          this.alternativas.push({
            idAlternativa: null,
            icono: 'ent-web-clean',
            descripcion: null,
            css: 'btn-card-red',
            esCorrecto: false,
          });
        } else {
          this.toast.showToast(
            'Sólo está permitido un máximo de ' +
              this.cantRespuestasPermitidas +
              ' respuestas',
            'danger',
            'Atención'
          );
        }
      });
  }

  onDescripcionChange(event: any, i: any) {
    this.alternativas[i].descripcion = event.target.value;
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

  onSelectionTipoPregunta($event) {
    this.f.rangoIncremental.setValue(null);

    if ($event !== '3') {
      this.viewAlternative = true;
      this.viewIncremento = false;
    } else {
      this.alternativas = [];
      this.viewAlternative = false;
      this.viewIncremento = true;
    }
  }
}
