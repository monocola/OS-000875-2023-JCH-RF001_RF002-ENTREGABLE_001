import { Component, HostListener, OnInit } from '@angular/core';
import {
  AbstractControl,
  FormBuilder,
  FormControl,
  FormGroup,
  ValidationErrors,
  ValidatorFn,
  Validators
} from '@angular/forms';
import { MatTableDataSource } from '@angular/material/table';
import { MatDialog } from '@angular/material/dialog';
import { NuevaEvidenciaComponent } from '../nueva-evidencia/nueva-evidencia.component';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { IParticipanteEvaluador } from 'src/app/@data/model/participante';
import { EvidenciaParticipante } from 'src/app/@data/model/meta';
import { EditarEvidenciaComponent } from '../editar-evidencia/editar-evidencia.component';
import { EliminarEvidenciaComponent } from '../eliminar-evidencia/eliminar-evidencia.component';
import { MetaRepository } from 'src/app/@domain/repository/meta.repository';
import { Utils } from '../../../../utils/utils';
import { NavigationStart, Router } from '@angular/router';
import { Location, PlatformLocation } from '@angular/common';
import { AlertabackComponent } from '../../../@common-components/alertaback/alertaback.component';

const ELEMENT_DATA: any[] = [];

@Component({
  selector: 'serv-talento-agregar-meta',
  templateUrl: './agregar-meta.component.html',
  styleUrls: ['./agregar-meta.component.scss']
})
export class AgregarMetaComponent implements OnInit {

  evidencia: EvidenciaParticipante[] = [];
  ordersTableColumns: TableColumn[];
  searchMode = false;
  form: FormGroup = null;
  estados: { id, value }[] = [];
  participante: IParticipanteEvaluador = {};
  ciclo = JSON.parse(sessionStorage.getItem('ciclo'));
  dataSource = new MatTableDataSource<EvidenciaParticipante>(ELEMENT_DATA);
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
  }

  cicloDefaultDesc;
  cicloDefault;

  ngOnInit(): void {
    this.initForm();
    this.setCiclo();
    this.listarEvidencias();
    this.metaRepository.estadosMeta()
      .subscribe(data => this.estados = data);

    if (this.ciclo?.cicloId) {
      if (this.participante.accedioDirecto === true) {
        this.listarEvidencias();
      }
    } else {
      this.toastService.showToast(
        'No se tiene seleccionado ningún ciclo',
        'danger'
      );
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
  }

  editEvidencia(data) {
    this.dialog.open(EditarEvidenciaComponent, {
      disableClose: true,
      data: data,
    }).afterClosed()
      .subscribe(value => {
        if (value.result) {
          this.evidencia = this.evidencia.map(item => {
            if (item.numeracion === value.data.numeracion) {
              item = value.data;
            }
            return item;
          });
          this.dataSource.data = this.evidencia;
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
          this.evidencia = this.evidencia.filter(item => item.numeracion !== value.posItem).map((item, index) => {
            item.numeracion = index + 1;
            return item;
          });
          this.dataSource.data = this.evidencia;
        }
      });
  }

  nuevaEvidencia() {
    this.dialog.open(NuevaEvidenciaComponent)
    .afterClosed()
    .subscribe(x => {
      if (x) {
          // const xx = [x];
        this.evidencia = [x, ...this.evidencia];
        this.evidencia = this.evidencia.map((item, index) => {
          item.numeracion = index + 1;
          return item;
        });
        this.dataSource.data = this.evidencia;
        this.toastService.showToast(
          'Se realizó el registro exitosamente',
          'success'
        );

      }
    });
  }

  listarEvidencias() {

  }

  private initForm() {
    this.form = this.fb.group({
      fuente: new FormControl(null, Validators.required),
      prioridad: new FormControl(null, Validators.required),
      tipo: new FormControl(null, Validators.required),
      detalle: new FormControl(null, Validators.required),
      valor_meta: new FormControl(null, Validators.required),
      peso: new FormControl(null, [Validators.required, this.pesoValidator(100 - this.participante.pesoToTal)]),
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
      });
  }

  guardarMetas() {
    if (this.preValidData()) {
      this.metaRepository.saveMeta(this.getParserDataSend())
        .subscribe(result => {
          if (result) {
            this.toastService.showToast(
              'Se registró de manera exitosa la meta',
              'success',
              "Registro de meta"
            );
            this.router.navigateByUrl('/pages/participantes/evaluados/metas');
          } else {
            this.toastService.showToast(
              'Ocurrió un error al guardar Meta',
              'danger',
              "Error"
            );
          }
        }, error => {
          this.toastService.showToast(
            'Ocurrió un error al guardar Meta',
            'danger',
            "Error"
          );
        });
    } else {
      this.toastService.showToast(
        'Sólo deberá de registrar mas de 2 evidencia',
        'danger',
        "Error"
      );
    }
  }

  preValidData(): boolean {
    return this.dataSource.data.length >= 2;
  }

  getParserDataSend() {
    let participante = this.metaRepository.getDataParticipante();
    let ciclo = this.metaRepository.getDataCiclo();
    let formdata = this.form.getRawValue();
    let estadoPendiente = this.estados.find(item => item.id === 1);
    return {
      personaId: Utils.nullToEmpty(participante.personaId),
      detaUoId: Utils.nullToEmpty(participante.detUnidadOrganicaId),
      cicloId: Utils.nullToEmpty(ciclo.cicloId),
      fuente: Utils.nullToEmpty(formdata.fuente),
      prioridad: Utils.nullToEmpty(formdata.prioridad),
      indicador: Utils.nullToEmpty(formdata.detalle),
      tipoIndicadorProducto: Utils.nullToEmpty(formdata.tipo),
      valorMeta: Utils.nullToEmpty(formdata.valor_meta),
      peso: Utils.nullToEmpty(formdata.peso),
      tipoMeta: Utils.nullToEmpty(formdata.tipo_valor),
      estadoMeta: estadoPendiente.id,
      descripcionEstado: estadoPendiente.value,
      descripcionObservacion: "",
      evidencia: this.dataSource.data.map(item => {
        return {
          metaId: item.numeracion,
          evidencia: item.evidencia,
          plazo: item.plazo,
        };
      })
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
}
