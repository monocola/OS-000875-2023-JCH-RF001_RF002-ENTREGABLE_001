import { Component, HostListener, OnInit } from '@angular/core';
import { EvidenciaParticipante } from '../../../../@data/model/meta';
import { TableColumn } from '../../../@common-components/material-table/table-column';
import {
  AbstractControl,
  FormBuilder,
  FormControl,
  FormGroup,
  ValidationErrors,
  ValidatorFn,
  Validators
} from '@angular/forms';
import { IParticipanteEvaluador } from '../../../../@data/model/participante';
import { MatTableDataSource } from '@angular/material/table';
import { MatDialog } from '@angular/material/dialog';
import { ToastService } from '../../../@common-components/toast';
import { MetaRepository } from '../../../../@domain/repository/meta.repository';
import { ActivatedRoute, NavigationStart, Params, Router } from '@angular/router';
import { Location, PlatformLocation } from '@angular/common';
import { EditarEvidenciaComponent } from '../editar-evidencia/editar-evidencia.component';
import { EliminarEvidenciaComponent } from '../eliminar-evidencia/eliminar-evidencia.component';
import { NuevaEvidenciaComponent } from '../nueva-evidencia/nueva-evidencia.component';
import { Utils } from '../../../../utils/utils';
import { AlertabackComponent } from '../../../@common-components/alertaback/alertaback.component';
import { forkJoin } from 'rxjs';

const ELEMENT_DATA: any[] = [];
@Component({
  selector: 'serv-talento-editar-meta',
  templateUrl: './editar-meta.component.html',
  styleUrls: ['./editar-meta.component.scss']
})
export class EditarMetaComponent implements OnInit {

  metaId: number;
  evidencia: any[] = [];
  ordersTableColumns: TableColumn[];
  searchMode = false;
  form: FormGroup = null;
  idMeta: number;
  data: any;
  participante: IParticipanteEvaluador = {};
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  dataSource = new MatTableDataSource<any>(ELEMENT_DATA);
  displayedColumns: string[] = [
    'numeracion',
    'evidencia',
    'plazo',
    'acciones'
  ];

  constructor(
    public dialog: MatDialog,
    private fb: FormBuilder,
    private toastService: ToastService,
    private metaRepository: MetaRepository,
    private router: Router,
    private location: Location,
    private activatedRoute: ActivatedRoute,
    private platformLocation: PlatformLocation
  ) {
    this.participante = this.metaRepository.getDataParticipante();
    platformLocation.onPopState(() => {
      this.back();
    });
    router.events
      .subscribe((event: NavigationStart) => {
        if (event.navigationTrigger === 'popstate') {
          this.back();
        }
      });

    this.activatedRoute.params.subscribe((params) => {
      if (params.metaId) {
        this.metaId = params.metaId;
      }
    });
  }

  cicloDefaultDesc;
  cicloDefault;

  ngOnInit(): void {
    this.loadCombox();
    this.setCiclo();

    if (this.ciclo?.cicloId) {
      if (this.participante.accedioDirecto === true) {
        return;
      }
    } else {
      this.toastService.showToast(
        'No se tiene seleccionado ningún ciclo',
        'danger'
      );
    }

    this.activatedRoute.params.subscribe((params: Params) => {
      if (params.idMeta) {
        this.idMeta = params.idMeta;
        this.metaRepository.detalleMeta(this.idMeta)
          .subscribe(data => {
            this.data = data;
            this.initForm();
            this.setDataMeta();
          });
      }
    });
  }

  loadCombox() {
    if (this.metaId) {
      const getDetaMeta = this.metaRepository.detalleMeta(this.metaId);
      forkJoin([getDetaMeta]).subscribe(
        (results) => {
          this.data = results[0];
          this.initForm();
          this.setDataMeta();
        });
    }
  }

  setCiclo() {
    if (this.ciclo.length !== 0 && this.ciclo) {
      this.cicloDefaultDesc = ' - ' + this.ciclo.anio;
      this.cicloDefault = this.ciclo.cronogramaId;
    } else {
      this.cicloDefaultDesc = '';
      this.cicloDefault = 0;
      this.toastService.showToast(
        'No se encuentra ciclo configurado',
        'danger'
      );
    }
    if (!this.ciclo?.cicloId) {
      this.toastService.showToast('No se tiene seleccionado ningún ciclo', 'danger' );
    }
    /*if (this.ciclo?.cicloId) {
      if (this.participante.accedioDirecto === true) {
        return;
      }
    } else {
      this.toastService.showToast(
        'No se tiene seleccionado ningún ciclo',
        'danger'
      );
    }*/
  }

  editEvidencia(data) {
    this.dialog.open(EditarEvidenciaComponent, {
      disableClose: true,
      data: data,
    }).afterClosed()
      .subscribe(value => {
        if (value.result) {
          this.evidencia = this.evidencia.map(item => {
            if (item.numeracion === value.data.numeracion && item.estadoRegistro !== "0") {
              item = value.data;
            }
            return item;
          });
          this.dataSource.data = this.evidencia.filter(item => item.estadoRegistro !== "0");
        }
      });
  }


  deleteEvidencia(dataToDelete: EvidenciaParticipante) {
    this.dialog.open(EliminarEvidenciaComponent, {
      disableClose: true,
      data: dataToDelete.numeracion,
    }).afterClosed()
      .subscribe(value => {
        if (value.result) {
          let pos = 1;
          this.evidencia = this.evidencia.map((item, index) => {
            if (item.numeracion === value.posItem && item.estadoRegistro !== "0") {
              item.estadoRegistro = "0";
            }
            if (item.estadoRegistro !== "0") {
              item.numeracion = pos;
              pos++;
            }
            return item;
          });
          this.dataSource.data = this.evidencia.filter(item => item.estadoRegistro !== "0");
        }
      });
  }

  nuevaEvidencia() {
    this.dialog.open(NuevaEvidenciaComponent)
      .afterClosed()
      .subscribe(x => {
        if (x) {
          this.evidencia = [x, ...this.evidencia];
          let pos = 1;
          this.evidencia = this.evidencia.map((item, index) => {
            if (item.estadoRegistro !== "0") {
              item.numeracion = pos;
              pos++;
            }
            return item;
          });
          this.dataSource.data = this.evidencia.filter(item => item.estadoRegistro !== "0");
          this.toastService.showToast(
            'Se realizó el registro exitosamente',
            'success'
          );

        }
      });
  }

  private initForm() {
    this.form = this.fb.group({
      fuente: new FormControl(null, Validators.required),
      prioridad: new FormControl(null, Validators.required),
      tipo: new FormControl(null, Validators.required),
      detalle: new FormControl(null, Validators.required),
      valor_meta: new FormControl(null, Validators.required),
      peso: new FormControl(null, [Validators.required, this.pesoValidator(100 - this.participante.pesoToTal + this.data?.peso)]),
      tipo_valor: new FormControl(null),
    });

    this.form.controls.tipo.valueChanges
      .subscribe(newvalue => {
        this.form.controls.tipo_valor.setValue(null);
        this.form.controls.tipo_valor.clearValidators();
        if (newvalue === 0) {
          this.form.controls.valor_meta.enable();
          this.form.controls.valor_meta.setValue(null);
          this.form.controls.tipo_valor.setValidators(Validators.required);
        } else {
          this.form.controls.valor_meta.disable();
          this.form.controls.valor_meta.setValue("1.00");
        }
        this.form.controls.tipo_valor.updateValueAndValidity();
        console.info("termino initform");
      });
  }

  actualizarMetas() {
    if (this.preValidData()) {
      this.metaRepository.updateMeta(this.getParserDataSend())
        .subscribe(result => {
          if (result) {
            this.toastService.showToast(
              'Se realizó la edición de manera exitosa.',
              'success',
              "Edición de meta"
            );
            this.router.navigateByUrl('/pages/participantes/evaluados/metas');
          } else {
            this.toastService.showToast(
              'No se pudo guardar los cambios aplicados, porfavor vuelva a intentarlo',
              'danger',
              "Error al editar"
            );
          }
        }, error => {
          this.toastService.showToast(
            'No se pudo guardar los cambios aplicados, porfavor vuelva a intentarlo',
            'danger',
            "Error al editar"
          );
        });
    } else {
      this.toastService.showToast(
        'Debería registrar al menos 2 evidencias',
        'danger',
        "Error de evidencia"
      );
    }
  }

  preValidData(): boolean {
    return this.dataSource.data.length >= 2;
  }

  getParserDataSend() {
    let formdata = this.form.getRawValue();
    return {
      personaId: this.data.personaId,
      detaUoId: this.data.detaUoId,
      cicloId: this.data.cicloId,
      fuente: Utils.nullToEmpty(formdata.fuente),
      prioridad: Utils.nullToEmpty(formdata.prioridad),
      indicador: Utils.nullToEmpty(formdata.detalle),
      tipoIndicadorProducto: Utils.nullToEmpty(formdata.tipo),
      valorMeta: Utils.nullToEmpty(formdata.valor_meta),
      peso: Utils.nullToEmpty(formdata.peso),
      tipoMeta: Utils.nullToEmpty(formdata.tipo_valor),
      estadoMeta: this.data.estadoMeta,
      descripcionEstado: this.data.descripcionEstado,
      descripcionObservacion: this.data.descripcionObservacion,
      evidencia: this.evidencia,
      estadoRegistro: this.data.estadoRegistro,
      metaId: this.data.metaId,
      numeracion: this.data.numeracion,
    };
  }

  back() {
    const confirmModal = this.dialog.open(AlertabackComponent, {
      data: {
        icon: 'pregunta',
        pack: 'entweb',
        bodyText: 'Al salir perderá todos los datos ingresados.\n¿Está realmente seguro de realizar la siguiente acción? ',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.router.navigateByUrl('/pages/participantes/evaluados/metas');
      }
    });

  }

  @HostListener('window:popstate', ['$event'])
  onPopState(event) {
    this.back();
  }
  pesoValidator(cant: number): ValidatorFn {
    return (control: AbstractControl): ValidationErrors | null => {
      return control.value <= cant ? null : {pesoValid: {value: control.value}};
    };
  }

  private setDataMeta() {
    this.form.patchValue({
      fuente: this.data.fuente,
      prioridad: this.data.prioridad,
      tipo: +this.data.tipoIndicadorProducto,
      detalle: this.data.indicador,
      valor_meta: this.data.valorMeta,
      peso: this.data.peso,
      tipo_valor: +this.data.tipoMeta,
    });
    this.evidencia = this.data.evidencia;
    this.dataSource.data = this.evidencia.filter(item => item.estadoRegistro !== 0);
    console.info("termino setDataMeta");
  }
}
