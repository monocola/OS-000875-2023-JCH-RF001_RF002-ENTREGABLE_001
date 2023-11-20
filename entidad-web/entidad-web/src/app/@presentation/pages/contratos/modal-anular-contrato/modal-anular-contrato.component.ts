import { Component, Inject, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialogRef, MAT_DIALOG_DATA } from '@angular/material/dialog';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import { Const } from 'src/app/@data/services/const';
import { getBase64 } from 'src/app/utils/converterFile';
import { MatDialog } from '@angular/material/dialog';
import { MaestraRepository } from 'src/app/@domain/repository/maestra.reposity';
import { DesestimientoRepository } from 'src/app/@domain/repository/desestimiento.repository';
import { SeleccionServirRepository } from 'src/app/@domain/repository/seleccion-servir-repository';
import moment from 'moment';
@Component({
  selector: 'serv-talento-modal-anular-contrato',
  templateUrl: './modal-anular-contrato.component.html',
  styleUrls: ['./modal-anular-contrato.component.scss'],
})
export class ModalAnularContratoComponent implements OnInit {
  Form: FormGroup;
  title: string = '';
  fileName: string = '';
  text: string = '';
  filepdf: string = '';
  pathpdf: string = '';
  urlpdf: string = null;
  aprobacion: boolean = false;
  perfiles = [];
  comunicados = [];
  estados = [];
  comunicado: any;
  today: moment.Moment = moment();
  fechaInicio: moment.Moment = moment();
  fechaFin: moment.Moment = moment();

  textObservacion: string = null;
  uploadfile: boolean = true;
  editable: boolean = false;
  const = Const;
  rol: number = 0;
  estadoConvocatoriaId: any = null;
  ContratoServiceService: any;
  TableColumns = [];
  estadoId: number;
  baseId: number;
  tipoTrabajo: string;
  postulante: any;
  contrato: any;
  lista = [];
  postulanteId: number;

  columns: TableColumn[];

  page: number = 0;
  size: number = 10;
  total: number = 0;
  envio: boolean = false;

  vacantes: number;
  califica: number;
  nocalifica: number;
  convocatoria: string;

  filtros = {
    perfil: 0,
    fecIni: '',
    fecFin: '',
    estado: 1,
    rnssc: 0,
    reqmin: 0,
  };

  constructor(
    private fb: FormBuilder,
    private dialogRef: MatDialogRef<ModalAnularContratoComponent>,
    private toast: ToastService,
    private dialog: MatDialog,
    private maestraService: MaestraRepository,
    private DesestimientoContratoService: DesestimientoRepository,
    private seleccionService: SeleccionServirRepository,
    @Inject(MAT_DIALOG_DATA) public data: any
  ) { }

  async ngOnInit() {
    this.dialogRef.updateSize('48%', '90%');
    this.title = 'Desestimar el contrato';
    this.initializeColumns();
    this.initializeForm();
    this.fileName = 'Subir Sustento';
    this.getCombox();
    this.getEstadoId();
    this.contrato = this.data.contratoId;
    this.tipoTrabajo = this.data.tipoTrabajo;
    this.baseId = this.data.baseId;
    this.postulanteId = this.data.postulanteId;
    this.getData();
  }

  get f() {
    return this.Form.controls;
  }

  initializeColumns() {
    this.TableColumns = [
      {
        name: 'ORDEN DE MÉRITO',
        dataKey: 'resultado',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name: 'NOMBRES Y APELLIDOS',
        dataKey: 'postulante',
        position: 'left',
        isSortable: true,
        width: '45%',
      },
    ];
  }

  initializeForm() {
    this.Form = this.fb.group({
      categoria: ['', Validators.required],
      motivo: ['', Validators.required],
    });
  }

  onFileSelected(event: any) {
    const file: File = event.target.files[0];
    const reqFileName = file.name.split('.')[0];
    const pattern = new RegExp(/^[A-Za-z0-9]+$/g);

    if (!pattern.test(reqFileName)) {
      this.toast.showToast(
        ' El nombre del archivo solo debe contener números y letras.',
        'warning',
        'Atención'
      );
    } else {
      this.fileName = file.name;
      getBase64(file).then((data: string) => {
        this.filepdf = data.split(',')[1];
        this.pathpdf = 'data:application/pdf;base64,' + this.filepdf;
        this.uploadfile = this.pathpdf !== '';
        this.urlpdf = null;
      });
    }
  }

  GuardarComunicado() {
    if (this.Form.valid) {
      this.onNoClick(this.f.observadorText.value);
    }
  }

  onNoClick(data = false) {
    this.dialogRef.close(data);
  }

  getEstadoId() {
    this.maestraService
      .getMaestraDetalleByCodandCodProg('TBL_EST_CONT_CONV', 4)
      .subscribe((res) => {
        this.estadoId = res[0].maeDetalleId;
      });
  }

  private getData() {
    this.seleccionService
      .getPostulantesByConvocatoria(
        this.baseId,
        this.filtros.perfil,
        this.filtros.estado,
        this.filtros.rnssc,
        this.filtros.reqmin,
        this.filtros.fecIni,
        this.filtros.fecFin,
        this.page,
        this.size
      )
      .subscribe((res) => {
        this.lista = res.items;
        this.nocalifica = res.noCalifican;
        this.califica = res.califican;
        this.vacantes = res.vacantes;
        this.total = res.total;
        let count = 1;
        this.lista.forEach((element) => {
          element.id = count++ + this.page * this.size;
          element.estadoPostulante = false;
          element.fechaPostulacion = moment(element.fechaPostulacion).format(
            'DD/MM/yyyy'
          );
          element.postulante =
            ((element.nombres + ' ' + element.apellidos).length > 20
              ? (element.nombres + ' ' + element.apellidos).substring(0, 20) +
              '... '
              : element.nombres + ' ' + element.apellidos + ',') +
            (element.tipoDocumento === 1 ? ' DNI' : ' CE') +
            ' ' +
            element.documento;
          element.postulanteCompleto =
            element.nombres + ' ' + element.apellidos;
          element.documentoCompleto =
            (element.tipoDocumento === 1 ? ' DNI' : ' CE') +
            ' ' +
            element.documento;
        });
        let lista2 = [];
        this.lista.forEach(item => {
          if (item.postulanteId !== this.postulanteId) {
            lista2.push(item);
          }
        });
      });
  }

  aceptarClick() {
    if (this.Form.valid) {

      let seleccionado = {
        baseId: this.postulante.baseId,
        postulanteId: this.postulante.postulanteSelId,
        perfilId: this.postulante.perfilId,
        entidadId: this.postulante.entidadId,
        contratoId: this.postulante.contrato,
      };

      if (
        this.f.categoria.value !== '' &&
        this.f.motivo.value !== '' &&
        this.filepdf !== ''
      ) {
        this.DesestimientoContratoService.DesestimarContrato(
          this.contrato,
          this.f.categoria.value,
          this.f.motivo.value,
          this.estadoId,
          this.filepdf,
          this.tipoTrabajo,
          seleccionado
        ).subscribe((res) => {
          this.onNoClick(true);
        });
      } else {
        this.toast.showToast(
          'Los campos y carga de archivo deben estar llenos.',
          'danger',
          'Atención'
        );
      }

    } else {
      this.toast.showToast(
        'Los campos y carga de archivo deben estar llenos.',
        'danger',
        'Atención'
      );

    }

  }

  postulanteSeleccionar(res: any) {
    this.postulante = res;
  }

  getCombox() {
    this.maestraService.getMaestraDetalleByCod('DES_CON').subscribe((res) => {
      this.estados = res;
    });
  }
}
