import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { Const } from 'src/app/@data/services/const';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { getBase64 } from 'src/app/utils/converterFile';
import { MatDialog } from '@angular/material/dialog';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { DesestimientoRepository } from 'src/app/@domain/repository/desestimiento.repository';
import moment from 'moment';

@Component({
  selector: 'serv-talento-modal-contrato',
  templateUrl: './modal-contrato.component.html',
  styleUrls: ['./modal-contrato.component.scss']
})

export class ModalContratoComponent implements OnInit {
  Form: FormGroup;
  message: boolean = false;
  title: string = '';
  fileName: string = 'Subir documento';
  filepdf: string = '';
  pathpdf: string = '';
  urlpdf: string = null;
  aprobacion: boolean = false;
  perfiles = [];
  comunicados = [];
  estados = [];
  comunicado: any;
  textObservacion: string = null;
  uploadfile: boolean = true;
  editable: boolean = false;
  text: string;
  const = Const;
  rol: number = 0;
  estadoConvocatoriaId: any = null;
  ContratoServiceService: any;
  estadoId: number;
  postulante: any;
  contrato: any;
  fechaStatus: string = 'basic';

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalContratoComponent>,
    private toast: ToastService,
    private dialog: MatDialog,
    private maestraService: MaestraRepository,
    private DesestimientoContratoService: DesestimientoRepository,
    @Inject(MAT_DIALOG_DATA) public data: any,
  ) { }

  async ngOnInit() {
    this.title = 'Subir Escaneado';
    this.initializeForm();
    this.fileName = 'Subir contenido';
    this.getEstadoId();
    this.contrato = this.data.contratoId;
  }

  get f() {
    return this.Form.controls;
  }

  initializeForm() {
    this.Form = this.fb.group({
      perfil: '',
      fecha: ''
    });
  }

  onFileSelected(event: any) {
    const file: File = event.target.files[0];
    const reqFileName = file.name.split('.')[0];
    const pattern = new RegExp(/^[A-Za-z0-9]+$/g);

    if (!pattern.test(reqFileName)) {
      this.toast.showToast(
        ' El nombre del archivo solo debe contener números y letras sin espacios.',
        'warning',
        'Atención'
      );
    } else {
      this.fileName = file.name;
      getBase64(file).then((data: string) => {
        this.filepdf = data.split(',')[1];
        this.pathpdf = 'data:application/msword;base64,' + this.filepdf;
        this.uploadfile = this.pathpdf !== '';
        this.urlpdf = null;
      });
    }
  }

  GuardarComunicado() {
    if (this.f.fecha.value !== '' && this.filepdf !== '') {
      this.DesestimientoContratoService.SuscribirContrato(
        this.contrato, this.estadoId, this.filepdf, this.f.fecha.value)
        .subscribe((res) => {
          this.onNoClick(true);
        });
    } else {
      this.toast.showToast('Los campos y carga de archivo deben estar llenos.', 'danger', 'Atención'
      );
    }
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }

  getEstadoId() {
    this.maestraService
      .getMaestraDetalleByCodandCodProg('TBL_EST_CONT_CONV', 3)
      .subscribe((res) => {
        this.estadoId = res[0].maeDetalleId;
      });
  }

  validDateFormat (event: any) {
    this.fechaStatus = moment (event).isValid () ? 'basic' : 'danger';
  }
}
