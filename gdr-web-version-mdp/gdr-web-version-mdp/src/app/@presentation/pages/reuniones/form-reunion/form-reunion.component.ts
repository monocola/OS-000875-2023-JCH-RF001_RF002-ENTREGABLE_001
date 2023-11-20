import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialog, MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { DataModel } from '../../organigrama/modal-creacion-uo/modal-creacion-uo.component';
import { Utils } from 'src/app/utils/utils';

import { Reuniones } from 'src/app/@data/model/reuniones';
import { ReunionesRepository } from 'src/app/@domain/repository/reuniones.repository';
import { Const } from 'src/app/@data/services/const';
import { DOCUMENT } from '@angular/common';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { forkJoin } from 'rxjs';
import { Genre } from 'src/app/@data/model/genre';
import { ModalNotificacionesComponent } from '../modal-notificaciones/modal-notificaciones.component';
import { ModalQuestionComponent } from '../../gestores-orh/modal-question/modal-question.component';
import { Router } from '@angular/router';
import { THIS_EXPR } from '@angular/compiler/src/output/output_ast';

@Component({
  selector: 'serv-talento-form-reunion',
  templateUrl: './form-reunion.component.html',
  styleUrls: ['./form-reunion.component.scss'],
})
export class FormReunionComponent implements OnInit {
  formReunion: FormGroup;
  dateReunion: Date;
  dateReunionFin: Date;
  dateReunionString: String;
  dateReunionInicioString: String;
  typeDocuments: Genre[];
  zonaHorariaReu: any[];
  zonaHorariaReuFilter: any[];
  urlGoogleMeet: string;

  constructor(
    private matDialog: MatDialogRef<FormReunionComponent>,
    private fb: FormBuilder,
    private dialog: MatDialog,
    private toastService: ToastService,
    private router: Router,
    @Inject(MAT_DIALOG_DATA) public data: DataModel,
    private reunionesRepository: ReunionesRepository,
    @Inject(DOCUMENT) private document: Document,
    private parameterRepository: ParameterRepository
  ) {}

  ngOnInit(): void {
    this.loadCombox();
    this.initializeForm();
    this.Closedate;
  }

  get f() { return this.formReunion.controls; }

  loadCombox() {
    const typeDocuments = this.parameterRepository.getTypeAgendamiento();
    const zonahorariareuniones = this.parameterRepository.getZonaHorariaReunion();

    forkJoin([typeDocuments]).subscribe(
      (results) => {
        this.typeDocuments = results[0];
      },
      (err) => {}
    );
    forkJoin([zonahorariareuniones]).subscribe(
      (results) => {
        this.zonaHorariaReu = results[0];
      },
      (err) => {}
    );
  }
  Closedate(){
    const myMaybeNullElement = window.document.getElementById("autoclick")
    myMaybeNullElement?.click();
  }

  initializeForm() {
    const tipoAgendamientoId = Const.TPO_AGENDAMIENTO_ID;
    this.formReunion = this.fb.group({
      fecha: ['', [Validators.required]],
      hora: ['', [Validators.required]],
      duracion: ['', [Validators.required]],
      tipoAgendamientoId: [tipoAgendamientoId, [Validators.required]],
    });

    setTimeout(() => {
      const input = document.getElementById('fecha');


      input.setAttribute('maxlength', '10');


      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e.keyCode);
      };


      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e.keyCode);
      };
    }, 250);

    if (!this.data.createMode) {
      if (this.data.dataToEdit.fechaReunion) {
        const fechaD = new Date(this.stringToDate(this.data.dataToEdit.fechaReunion));
        this.formReunion.patchValue({fecha : fechaD});
      }

      if (this.data.dataToEdit.tipoAgendamientoId) {
        this.formReunion.patchValue({
          tipoAgendamientoId: this.data.dataToEdit.tipoAgendamientoId,
        });
      }
      if (this.data.dataToEdit.horaReunion) {
        this.formReunion.patchValue({
          hora : new Date(
              Date.parse( "2019-01-01T" + this.data.dataToEdit.horaReunion)
            ),
        });
      }
      if (this.data.dataToEdit.duracion) {
        this.formReunion.patchValue({
          duracion : this.data.dataToEdit.duracion,
        });
      }
    }
  }

  stringToDate(dateString) {
    const [day, month, year] = dateString.split('/');
    return new Date([month, day, year].join('/'));
  }

  cerrar(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  isShift: boolean = false;
  seperator: string = '/';
  isNumeric(input: any, keyCode: any) {
    console.log(keyCode);

    if (keyCode === 16) {
      this.isShift = true;
    }

    if (
      ((keyCode >= 48 && keyCode <= 57) ||
        keyCode === 8 ||
        keyCode === 46 ||
        keyCode === 37 ||
        keyCode === 39 ||
        (keyCode >= 96 && keyCode <= 105)) &&
      this.isShift === false
    ) {
      if (
        (input.value.length === 2 || input.value.length === 5) &&
        keyCode !== 8 && keyCode !== 46
      ) {
        input.value += this.seperator;
      }

      return true;
    } else {
      return false;
    }
  }

  validateDateFormat(input, keyCode) {
    let dateString = input.value;
    if (keyCode === 16) {
      this.isShift = false;
    }
    let regex = /(((0|1)[0-9]|2[0-9]|3[0-1])\/(0[1-9]|1[0-2])\/((19|20)\d\d))$/;

    // Check whether valid dd/MM/yyyy Date Format.
    if (regex.test(dateString) || dateString.length === 0) {
      // Es valido
    } else {
      // Es invalido
    }
  }

  cancelar(flag: boolean = false) {
    const add = this.dialog.open(ModalQuestionComponent, {
      data: {
        title: 'Cancelar reunión',
        bodyText:
          'Se cancelará la reunión <br> ¿Está realmente seguro de realizar la siguiente acción?',
        rutaImagen: './assets/images/question.png',
        textCancel: 'NO',
        textOk: 'SI',
      },
    });
    add.afterClosed().subscribe((res) => {
      if (res) {
        this.matDialog.close(flag);
        this.reunionesRepository
          .cancelarReunion(this.data.dataToEdit.reunionId)
          .subscribe(
            (res) => {
              if (res) {
                this.toastService.showToast(
                  'La reunión se ha cancelado exitosamente',
                  'primary',
                  'Reunión cancelada'
                );
                this.redirectHistorial();
              } else {
                this.toastService.showToast('No se puede cancelar', 'primary');
              }
            },
            (err) => {
              this.toastService.showToast(err.message, 'danger');
            }
          );
        this.cerrar(true);
      }
    });
  }

  registrar() {
    this.formReunion.markAllAsTouched();
    if (this.formReunion.valid) {
      let duracion = new Date(this.formReunion.get('duracion').value);
      this.zonaHorariaReuFilter= this.zonaHorariaReu.filter(
        (item) =>
        item.codigoNumero === this.formReunion.get('duracion').value
      )
      let fecha = Utils.formatFechaDate(
        this.formReunion.get('fecha').value,
        'DD/MM/YYYY'
      );
      let hora = Utils.formatFechaDate(
        this.formReunion.get('hora').value,
        'HH:mm'
      );
      let duracionMinSeg = this.zonaHorariaReuFilter[0].valorTexto
      this.dateReunion = this.formReunion.get('fecha').value;

      if (hora) {
        this.dateReunion.setHours(Utils.horasDate(hora));
        this.dateReunion.setMinutes(Utils.minutosDate(hora));
      }

      this.dateReunionInicioString = Utils.getfechaDateMeet(this.dateReunion);

      this.dateReunionFin = this.dateReunion;

      if (duracionMinSeg) {
        this.dateReunionFin.setHours(
          this.dateReunionFin.getHours() + Utils.horasDate(duracionMinSeg)
        );
        this.dateReunionFin.setMinutes(
          this.dateReunionFin.getMinutes() + Utils.minutosDate(duracionMinSeg)
        );
      }

      this.dateReunionString = Utils.getfechaDateMeet(this.dateReunionFin);
      this.urlGoogleMeet =
        Const.URL_GOOGLE_MEET +
        Utils.generarUrlMeet(
          Utils.generarFechaMeet(this.dateReunionInicioString),
          Utils.generarFechaMeet(this.dateReunionString)
        );

      // let timeDuration = duracion.getMinutes() + duracion.getHours() * 60;
      // let fecha = Utils.formatFechaDate(
      //   this.formReunion.get('fecha').value,
      //   'DD/MM/YYYY'
      // );

      let tipo = 'N';

      const registerDialog = this.dialog.open(ModalNotificacionesComponent, {
        data: {
          tipo: tipo,
        },
        width: '38.375rem',
      });
      registerDialog.afterClosed().subscribe((res) => {

        if (res) {

          const datos: Reuniones = {
            cicloId: this.data.dataToEdit.cicloId,
            detaUoId: this.data.dataToEdit.detaUoId,
            evaluadoDetalleUoId: this.data.dataToEdit.evaluadoDetalleUoId,
            evaluadoPersonaId: this.data.dataToEdit.evaluadoPersonaId,
            fechaReunion: fecha,
            horaReunion: hora,
            duracion: this.formReunion.get('duracion').value,
            tipoAgendamientoId: this.f.tipoAgendamientoId.value,
            enviaraNotificacion: 1,
            esEvaluado: false,
          };

          if (this.data.createMode) {
            this.reunionesRepository.agregarReunion(datos).subscribe(
              (x) => {
                console.log("SE CREO1")
                this.toastService.showToast(
                  'La reunión se ha programado exitosamente',
                  'success',
                  'Reunión programada'
                );
                this.openGoogleMeetForm(datos.tipoAgendamientoId);
                this.cerrar(true);
                this.redirectHistorial();
              },
              (err) => this.toastService.showToast(err, 'danger')
            );
          } else {
            datos.reunionId = this.data.dataToEdit.reunionId;

            this.reunionesRepository.editarReunion(datos).subscribe(
              (x) => {
                this.toastService.showToast(
                  'La reunión se ha reprogramado exitosamente',
                  'success',
                  'Reunión reprogramada'
                );
                this.openGoogleMeetForm(datos.tipoAgendamientoId);
                this.cerrar(true);
                this.redirectHistorial();
              },
              (err) => this.toastService.showToast(err, 'danger')
            );
          }
        }
      });
    }
  }

  openGoogleMeetForm(tipoAgendamientoId: number) {
    var dataTipoAgendamiento = this.typeDocuments.find(x=> x.parametroId === tipoAgendamientoId)
    if (dataTipoAgendamiento.codigoTexto !== "SIN_ANGENDAR") {
      const link = this.document.createElement('a');
      link.target = '_blank';
      link.href = this.urlGoogleMeet;
      link.click();
      link.remove();
    }
  }

  duracionString(dura: number) {
    if (dura) {
      let horas = Math.floor(dura / 60);
      let minutos = dura % 60;
      let horasStr = horas.toString().length === 1 ? '0' + horas : horas;
      let minutosStr = minutos.toString().length === 1 ? '0' + minutos : minutos;
      return horasStr + ':' + minutosStr;
    } else {
      return '--:--';
    }
  }

  redirectHistorial() {
    sessionStorage.setItem(
      'nombreEvaluado',
      this.data.dataToEdit?.nombreEvaluado
    );
    sessionStorage.setItem(
      'dataEvaluado',
      JSON.stringify(this.data.dataToEdit)
    );
    this.router.navigate([
      '/pages/reuniones/' +
        this.data.dataToEdit?.evaluadoDetalleUoId +
        '/' +
        this.data.dataToEdit?.detaUoId,
    ]);
  }
}
