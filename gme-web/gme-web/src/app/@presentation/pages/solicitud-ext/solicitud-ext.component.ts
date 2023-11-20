import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { SolicitudExternaRepository } from '../../../@domain/repository/solicitud.externa.repository';
import { forkJoin } from 'rxjs';
import { ToastService } from '../../@common-components/toast';
import { ParameterRepository } from '../../../@domain/repository/parameter.repository';
import { TableColumn } from '../../@common-components/material-table/table-column';
import { Router } from '@angular/router';
import { NotificarSolicitudExtComponent } from './notificar-solicitud-ext/notificar-solicitud-ext.component';
import { MatDialog } from '@angular/material/dialog';
import { ConfiguracionRepository } from '../../../@domain/repository/configuracion.repository';

@Component({
  selector: 'gme-web-solicitud-ext',
  templateUrl: './solicitud-ext.component.html',
  styleUrls: ['./solicitud-ext.component.scss'],
})
export class SolicitudExtComponent implements OnInit {
  frm: FormGroup = null;
  anioFilter = null;
  anio: any;
  fecha = new Date();
  estado: any;
  tableColumns: TableColumn[];
  lstSolicitudesExt: any;
  body: any;
  rucMaxLenght = 11;
  simpleCharacteresAndNumber: string = 'integer';
  filter: boolean = false;

  constructor(
    private fb: FormBuilder,
    private dialog: MatDialog,
    private solicitudExRepository: SolicitudExternaRepository,
    private parameterRepository: ParameterRepository,
    private toastService: ToastService,
    private router: Router,
    private configuracionRepository: ConfiguracionRepository
  ) {}

  get f() {
    return this.frm.controls;
  }

  ngOnInit(): void {
    this.anioFilter = this.fecha.getFullYear();
    this.body = { anio: this.anioFilter };
    this.loadCombox();
    this.initForm();
    this.initializeColumns();
  }

  loadCombox() {
    const getEstado = this.parameterRepository.getEstadoSolExt();
    const getAnios = this.solicitudExRepository.getAnio();
    const getSolicitud = this.solicitudExRepository.getSolicitudExt(this.body);
    forkJoin([getEstado, getAnios, getSolicitud]).subscribe(
      (result) => {
        this.estado = result[0];
        this.anio = result[1];
        this.lstSolicitudesExt = result[2];
      },
      (err) => this.toastService.showToast(err, 'danger')
    );
  }

  initForm() {
    this.frm = this.fb.group({
      anio: [this.anioFilter.toString(), [Validators.required]],
      ruc: [''],
      estado: [''],
      nroDoc: [''],
      entidad: [''],
      nombres: [''],
    });
  }

  initializeColumns() {
    this.tableColumns = [
      {
        name: 'Fecha de solicitud',
        dataKey: 'fechaSolicitud',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'N° de Ruc',
        dataKey: 'rucEntidad',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Nombre de entidad',
        dataKey: 'razonSocial',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'DNI / CE',
        dataKey: 'numeroDocumento',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Solicitante',
        dataKey: 'nombreCompleto',
        position: 'left',
        isSortable: true,
      },
      {
        name: 'Estado',
        dataKey: 'colorEstado',
        innerHtml: true,
        isSortable: true,
      },
    ];
  }

  search() {
    let anio,
      rucEntidad,
      estado,
      numeroDocumento,
      razonSocial,
      nombreCompleto = null;
    if (this.frm.value.anio) {
      anio = this.frm.value.anio;
    }
    if (this.frm.value.ruc) {
      rucEntidad = this.frm.value.ruc;
    }
    if (this.frm.value.estado) {
      estado = this.frm.value.estado;
    }
    if (this.frm.value.nroDoc) {
      numeroDocumento = this.frm.value.nroDoc;
    }
    if (this.frm.value.entidad) {
      razonSocial = this.frm.value.entidad;
    }
    if (this.frm.value.nombres) {
      nombreCompleto = this.frm.value.nombres;
    }
    this.body = {
      anio: anio,
      rucEntidad: rucEntidad,
      estado: estado,
      numeroDocumento: numeroDocumento,
      razonSocial: razonSocial,
      nombreCompleto: nombreCompleto,
    };
    if (this.frm.valid) {
      this.solicitudExRepository
        .getSolicitudExt(this.body)
        .subscribe((item) => {
          this.lstSolicitudesExt = item;
        });
    }
  }

  clear() {
    this.ngOnInit();
  }

  openVer(row: any) {
    this.router.navigate([
      `pages/solicitudesExt/detalleSolicitudesExt/${row.solicitudEntidadExtId}`,
    ]);
  }

  notificar(solicitud) {
    if (solicitud.estadoId < 2) {
      this.toastService.showToast(
        'La solicitud aún no ha sido evaluada',
        'info'
      );
    } else {
      this.dialog
        .open(NotificarSolicitudExtComponent, {
          data: {
            solicitud: solicitud,
          },
          disableClose: true,
        })
        .afterClosed()
        .subscribe((any) => {
          if (any) {
            this.envioCorreo(any);
          }
        });
    }
  }

  envioCorreo(trama: any[]) {
    let nroItera = 1;
    if (trama.length > 0) {
      const tramaFilter = trama.filter(x => x.body.payload.data.to);
      tramaFilter.forEach((item) => {
        console.info(item.body.payload.data.to);
        if (item.body.payload.data.to !== null) {
          this.configuracionRepository
            .enviaCorreoMasivo(item.body)
            .subscribe((response) => {
              let confirma = null;
              let mensaje = null;
              if (response.payload.estado) {
                confirma = response.payload.estado;
                mensaje = 'Se notificó al solicitante por correo';
              } else {
                confirma = 'danger';
                mensaje = response.status.error.messages[0].message;
              }
              if (nroItera === tramaFilter.length) {
                this.toastService.showToast(
                  mensaje,
                  confirma,
                  'Notificación exitosa'
                );
              }
              nroItera = nroItera + 1;
            });
        }
      });
    }
    this.loadCombox();
  }
}
