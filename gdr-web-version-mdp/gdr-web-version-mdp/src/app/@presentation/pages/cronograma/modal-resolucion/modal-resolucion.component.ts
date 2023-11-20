import { Component, OnInit } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { MatDialogRef } from '@angular/material/dialog';
import { ToastService } from '../../../@common-components/toast';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { forkJoin } from 'rxjs';
import { UtilRepository } from '../../../../@domain/repository/util.repository';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';
import { HttpEventType, HttpResponse } from '@angular/common/http';
import moment from 'moment';

@Component({
  selector: 'serv-talento-modal-resolucion',
  templateUrl: './modal-resolucion.component.html',
  styleUrls: ['./modal-resolucion.component.scss'],
})
export class ModalResolucionComponent implements OnInit {
  filterForm: FormGroup;
  roleslistSelect: any[] = [];
  roleslist: any[] = [];
  responsableSString: string;
  responsableBString: string[] = [];
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  file: File;
  conteoupload: number = 0;

  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<ModalResolucionComponent>,
    private toastService: ToastService,
    private maeParametroRepository: MaestraParametroRepository,
    private utilService: UtilRepository,
    private authenticationService: AuthenticationRepository
  ) {}
  profile = this.authenticationService.getCurrentUserValue;
  responsable: MaestraParametro[];
  responsableslistSelect: MaestraParametro[] = [];

  ngOnInit(): void {
    this.initializeForm();
    this.loadCombox();
  }

  get f() {
    return this.filterForm.controls;
  }

  initializeForm() {
    this.filterForm = this.fb.group({
      nombreArchivo: ['', [Validators.required]],
      fechaApro: ['', [Validators.required]],
      responsables: [[], [Validators.required]],
      cicloId: [''],
    });

    setTimeout(() => {
      const input = document.getElementById('fechaApro');


      input.setAttribute('maxlength', '10');


      input.onkeydown = (e: any) => {
        return this.isNumeric(input, e.keyCode);
      };


      input.onkeyup = (e: any) => {
        this.validateDateFormat(input, e.keyCode);
      };
    }, 250);
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

  loadCombox() {
    const getResponsable = this.maeParametroRepository.getMaestraParametro(
      'RESPONSABLE_GDR'
    );
    forkJoin([getResponsable]).subscribe(
      (results) => {
        this.responsable = results[0];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  cambioResponsables(values: number[]) {
    this.responsableSString = '';
    this.responsableBString = [];

    this.responsableslistSelect = this.responsable.filter((item) =>
      values.includes(item.parametroId)
    );
    this.responsableslistSelect.forEach((item) => {
      this.responsableBString.push(item.valorTexto + ' | ');
      this.responsableSString = ''.concat(...this.responsableBString);
      this.responsableSString = this.responsableSString.substring(
        0,
        this.responsableSString.length - 3
      );
    });
  }

  onNoClick(flag: boolean = false) {
    this.matDialog.close(flag);
  }

  cambioRoles(values: number[]) {
    this.roleslistSelect = this.roleslist.filter((item) =>
      values.includes(item.rolId)
    );
  }

  saveResolucion() {
    this.filterForm.markAllAsTouched();
    if (this.filterForm.valid && this.file) {
      const bodyParam = {
        entidadId: this.profile.entidadId,
        cronogramaId: this.ciclo.cronogramaId,
        descripcionResolucion: this.filterForm.value.nombreArchivo,
        fechaAprobacion: this.filterForm.value.fechaApro,
        descripcionResponsable: this.responsableSString,
        cicloId: this.ciclo.cicloId,
      };

      bodyParam.fechaAprobacion = moment (bodyParam.fechaAprobacion).format ('DD[/]MM[/]YYYY');

      this.utilService
        .registrarResolucion(bodyParam, this.file)
        .subscribe((response: any) => {
          console.log(this.file);
          if (response.type === HttpEventType.UploadProgress) {
            this.conteoupload = Math.round(
              (100 * response.loaded) / response.total
            );
          } else if (response instanceof HttpResponse) {
            if (!response.body.status.success) {
              this.toastService.showToast(
                response.body.status.error.messages[0],
                'danger'
              );
            } else {
              this.toastService.showToast(
                'Se registro la resolución con Exito',
                'success',
                'Atención'
              );
            }

            this.onNoClick(true);
          }
        });
    } else {
      this.toastService.showToast(
        'Ingrese los campos obligatorios y/o Archivo Pdf',
        'danger'
      );
    }
  }

  archivoSeleccionado(events) {
    if (events.target.files.length === 1) {
      if (events.target.files[0].size <= 1101899) {
        if (events.target.files[0].type === 'application/pdf') {
          this.file = events.target.files[0];
        } else {
          this.toastService.showToast(
            'Subir un archivo con extensión PDF',
            'danger'
          );
        }
      } else {
        this.toastService.showToast(
          'Ingrese un archivo PDF menor a 1 MB',
          'danger'
        );
      }
    } else {
      this.toastService.showToast(
        'Solo esta permitido subir un archivo por registro',
        'danger'
      );
    }
  }

  deleteFile() {
    this.file = null;
  }
}
