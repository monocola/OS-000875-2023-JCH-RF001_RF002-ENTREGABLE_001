import { Component, Inject, OnInit } from '@angular/core';
import { FormGroup, FormBuilder, Validators } from '@angular/forms';
import { MAT_DIALOG_DATA, MatDialogRef } from '@angular/material/dialog';
import { ToastService } from '../../../@common-components/toast';
import { MaestraParametroRepository } from '../../../../@domain/repository/maestra-parametro.repository';
import { MaestraParametro } from '../../../../@data/model/maestra-parametro';
import { forkJoin } from 'rxjs';
import { UtilRepository } from '../../../../@domain/repository/util.repository';
import { AuthenticationRepository } from '../../../../@domain/repository/authentication.repository';
import { HttpEventType, HttpResponse } from '@angular/common/http';
import { IParticipanteEvaluador } from 'src/app/@data/model/participante';


@Component({
  selector: 'serv-talento-modal-documento',
  templateUrl: './modal-documento.component.html',
  styleUrls: ['./modal-documento.component.scss']
})
export class ModalDocumentoComponent implements OnInit {

  participante: IParticipanteEvaluador = {};
  filterForm: FormGroup;
  roleslistSelect: any[] = [];
  roleslist: any[] = [];
  responsableSString: string;
  responsableBString: string[] = [];
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  file: File;
  conteoupload: number = 0;
  personaId = null;


  constructor(
    private fb: FormBuilder,
    private matDialog: MatDialogRef<ModalDocumentoComponent>,
    private toastService: ToastService,
    private maeParametroRepository: MaestraParametroRepository,
    private utilService: UtilRepository,
    private authenticationService: AuthenticationRepository,
    private  servidoresRepository: UtilRepository,
    private authService: AuthenticationRepository,
    @Inject(MAT_DIALOG_DATA) public data: ModalParticipanteComponent
  ) {
    this.participante = JSON.parse(
      sessionStorage.getItem('selected_participante')
    );
  }
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
      nombreArchivo: [''],
      fechaApro: [''],
      responsables: [ [], [Validators.required]],
    });
  }


  loadCombox() {
    const getResponsable = this.maeParametroRepository.getMaestraParametro('RESPONSABLE_GDR');
    forkJoin([getResponsable]).subscribe(
      (results) => {
        this.responsable = results[0];
        console.info(this.data);
        console.info(this.data.participante.personaId);
        console.info(this.data.participante.detUnidadOrganicaId);
        this.personaId = this.data.participante.personaId;
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  cambioResponsables(values: number[]) {
    this.responsableSString = "";
    this.responsableBString = [];

    this.responsableslistSelect = this.responsable.filter((item) =>
      values.includes(item.parametroId)
    );
    this.responsableslistSelect.forEach(item => {
      this.responsableBString.push(item.valorTexto + ' | ');
      this.responsableSString = "".concat(...this.responsableBString);
      this.responsableSString = this.responsableSString.substring(1, this.responsableSString.length - 3);
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

  saveDocumento() {
    const entidadId = this.authService.getCurrentUserValue.entidadId;

    if ( this.file ) {
      const bodyParam = {
        cicloId: this.data.ciclo.cicloId,
        entidadId: entidadId,
        personaId: this.data.participante.personaId,
        detaUoId: this.data.participante.detUnidadOrganicaId,
      };
      this.servidoresRepository.subirFormatoMetas( bodyParam , this.file )
        .subscribe( (response: any) => {
          if (response.type === HttpEventType.UploadProgress) {
            this.conteoupload = Math.round(
              (100 * response.loaded) / response.total
            );
          } else if (response instanceof HttpResponse) {
            if ( !response.body.status.success ) {
              this.toastService.showToast(response.body.status.error.messages[0], 'danger');
            } else {
              this.toastService.showToast('Se cargo el documento de metas con Exito', 'success', 'Atención');
            }
          }
          this.onNoClick(true);
      });
    } else {
      this.toastService.showToast('Ingrese los campos obligatorios y/o Archivo Pdf', 'danger');
    }
  }

  archivoSeleccionado(events) {
    if (events.target.files.length === 1 ) {
      if ( events.target.files[0].size <= 1101899 ) {
        if ( events.target.files[0].type === 'application/pdf' ) {
          this.file = events.target.files[0];
        } else {
          this.toastService.showToast('Subir un archivo con extensión PDF', 'danger');
        }
      } else {
        this.toastService.showToast('Ingrese un archivo PDF menor a 1 MB', 'danger');
      }
    } else {
      this.toastService.showToast('Solo esta permitido subir un archivo por registro', 'danger');
    }
  }

  deleteFile() {
    this.file = null;
  }
}
export interface ModalParticipanteComponent {
  participante: IParticipanteEvaluador;
  ciclo: any;
}
