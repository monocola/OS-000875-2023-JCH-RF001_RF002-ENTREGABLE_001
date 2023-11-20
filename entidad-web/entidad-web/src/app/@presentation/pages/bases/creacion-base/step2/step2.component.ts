import { Component, OnInit } from '@angular/core';
import { FormBuilder, FormGroup, Validators } from '@angular/forms';
import { MatDialog } from '@angular/material/dialog';
import { Sort } from '@angular/material/sort';
import { Const } from 'src/app/@data/services/const';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { FileVisualizerComponent } from 'src/app/@presentation/@common-components/file-visualizer/file-visualizer.component';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
import { ModalConfirmationComponent } from 'src/app/@presentation/@common-components/modal-confirmation/modal-confirmation.component';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { sortDataTableComponent } from 'src/app/utils/general';
import { CreacionBaseService } from '../creacion-base.service';
import { ModalHorariosVacantesComponent } from './modal-horarios-vacantes/modal-horarios-vacantes.component';
import { ExportExcelModel } from '../../../../@service/export-excel.service';
import { AuthenticationRepository } from '../../../../../@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-step2',
  templateUrl: './step2.component.html',
  styleUrls: ['./step2.component.scss'],
})
export class Step2Component implements OnInit {
  registerForm: FormGroup = null;
  vacantes = [];
  vacantesColumns: TableColumn[];
  editMode = false;
  const = Const;
  vacantesTemp: number = 0;

  constructor(
    public helperService: CreacionBaseService,
    private basesService: BasesRepository,
    private fb: FormBuilder,
    private dialog: MatDialog,
    private toastService: ToastService,
    public authRepository: AuthenticationRepository
  ) {}

  ngOnInit(): void {
    this.initializeForm();
    this.initializeColumns();
    if (this.helperService.disableAllFields) {
      this.registerForm.disable();
    }
  }

  initializeForm() {
    this.registerForm = this.fb.group({
      idBasePerfil: null,
      perfil: ['', Validators.required],
      jornada: ['', Validators.required],
      vacantes: ['', Validators.required],
      remuneracion: ['', Validators.required],
      contratoIndeterminado: [false, Validators.required],
      duracionContrato: ['', Validators.required],
      condicion: ['', Validators.required],
      modalidadContrato: ['', Validators.required],
      horarios: [[], Validators.required],
      horariosToDelete: [[]],
      sede: ['', Validators.required],
      condicionesEscenciales: ['', Validators.required],
      observaciones: [''],
    });
    const regimenesPermitidos = [Const.MD_DL30057, Const.MD_DL728];
    const codProg = this.helperService.jerarquiaSelected.regimen.codProg;
    !regimenesPermitidos.includes(codProg)
      ? this.registerForm.get('modalidadContrato').disable()
      : this.registerForm.controls.modalidadContrato.enable();
  }

  get f() {
    return this.registerForm.controls;
  }
  get g() {
    return this.helperService.form1.controls;
  }

  validarVacante(registerForm: any, numeroVacantes: any, vacantes_list: any) {
    let vacante = Number(registerForm.vacantes);
    let vacantes: any[] = vacantes_list;

    if (registerForm.idBasePerfil !== null) {
      vacantes = vacantes.filter((item: any) => {
        return item.basePerfilId !== registerForm.idBasePerfil;
      });
    }

    vacantes.forEach((item: any) => {
      vacante += item.nroVacante;
    });

    return vacante <= numeroVacantes;
  }

  save() {
    if (
      this.validarVacante(
        this.registerForm.getRawValue(),
        Number(this.g.numeroVacantes.value),
        this.helperService.form2.controls.vacantes.value
      )
    ) {
      this.registerForm.markAllAsTouched();
      if (this.registerForm.invalid || this.f.horarios.value.length === 0)
        return;
      const body = this.registerForm.getRawValue();
      let bodySede = this.helperService.sedes.filter(
        (s) => s.sedeId === body.sede.sedeId
      );
      this.basesService
        .saveOrUpdateStep2(
          body,
          this.helperService.idBase,
          body.idBasePerfil,
          bodySede
        )
        .subscribe((res) => {
          if (this.editMode) {
            this.toastService.showToast(
              'Se guardaron los cambios correctamente',
              'success'
            );
          } else {
            this.toastService.showToast(
              'La vacante ha sido creada correctamente',
              'success'
            );
          }
          this.getPerfilesVacantes(this.helperService.idBase);
          this.initializeForm();
          this.editMode = false;
        });
    } else {
      this.toastService.showToast(
        'El número de vacantes es excedente o esta vacio',
        'warning',
        'Atención'
      );
    }
  }

  clear() {
    this.initializeForm();
  }

  openModalHorario() {
    const dialog = this.dialog.open(ModalHorariosVacantesComponent, {
      data: {
        horarios: this.registerForm.get('horarios').value,
      },
      width: '60rem',
    });
    dialog.afterClosed().subscribe((res) => {
      if (res) {
        this.registerForm.patchValue({
          horarios: res.horarios,
          horariosToDelete: this.f.horariosToDelete.value.concat(
            res.horariosToDelete
          ),
        });
      }
    });
  }

  // ** PARA LA TABLA ** //

  openPerfilPDF(e) {
    this.basesService.getPDFPerfil(e.perfilId).subscribe((res) => {
      const base64Data = res;
      this.dialog.open(FileVisualizerComponent, {
        data: {
          base64String: base64Data,
          filename: e.perfilNombre,
          extension: 'pdf',
        },
      });
    });
  }

  editVacante(event) {
    this.vacantesTemp = 0;
    this.helperService.form2.controls.vacantes.value.forEach((element) => {
      this.vacantesTemp += element.nroVacante;
    });
    this.vacantesTemp = this.vacantesTemp - event.nroVacante;
    this.editMode = true;
    this.basesService
      .getIndividualPerfilBase(event.basePerfilId)
      .subscribe((res) => {
        this.setData(res, 0);
      });
  }

  setData(res, mode) {
    setTimeout(() => {
      this.registerForm.patchValue({
        idBasePerfil: mode === 0 ? res.basePerfilId : null,
        perfil: this.helperService.perfiles.filter(
          (p) => p.perfilId === res.perfilId
        )[0],
        jornada: res.jornadaLaboral,
        vacantes: res.vacante,
        remuneracion: res.remuneracion,
        contratoIndeterminado:
          res.indContratoIndeterminado === '1' ? true : false,
        duracionContrato: res.tiempoContrato.toString(),
        condicion: res.condicionTrabajoId,
        modalidadContrato: res.modalidadContratoId || '',
        horarios: this.setHorarios(res.baseHorarioDTOList, mode),
        horariosToDelete: [],
        sede: this.helperService.sedes.find((s) => s.sedeId === res.sedeId),
        condicionesEscenciales: res.condicionEsencial,
        observaciones: res.observaciones || '',
      });
    }, 0);
  }

  setHorarios(horarios: any[], mode) {
    // mode: 0 == editar / 1 === duplicar
    const formatted = horarios.map((h, index) => {
      return {
        dia: this.helperService.frecuencias.find(
          (f) => f.maeDetalleId === h.frecuenciaId
        ).descripcion,
        diaId: h.frecuenciaId,
        horaFin: h.horaFin,
        horaInicio: h.horaIni,
        id: mode === 0 ? h.baseHorarioId : null,
        orden: index,
      };
    });
    return formatted;
  }

  removeVacante(event) {
    const confirmModal = this.dialog.open(ModalConfirmationComponent, {
      data: {
        title: 'Deshabilitar perfil/vacante',
        bodyText: '¿Está seguro de continuar?',
      },
    });
    confirmModal.afterClosed().subscribe((res) => {
      if (res === true) {
        this.basesService
          .deleteIndividualPerfilBase(event.basePerfilId)
          .subscribe(() => {
            this.getPerfilesVacantes(event.baseId);
          });
      }
    });
  }

  getPerfilesVacantes(baseId: number) {
    this.basesService
      .getDataStep2(baseId)
      .subscribe((res) => this.helperService.setStepTwo(res));
  }

  copiarVacante(event) {
    this.editMode = false;
    this.basesService
      .getIndividualPerfilBase(event.basePerfilId)
      .subscribe((res) => {
        this.setData(res, 1);
      });
  }

  sortData(sortParameters: Sort) {
    sortDataTableComponent(sortParameters, this.vacantes);
  }

  initializeColumns() {
    this.vacantesColumns = [
      {
        name: 'CÓDIGO',
        dataKey: 'codigoPuesto',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
      {
        name: 'VACANTES',
        dataKey: 'nroVacante',
        position: 'left',
        isSortable: true,
        width: '8%',
      },
      {
        name: 'LUGAR',
        dataKey: 'sedeDireccion',
        position: 'left',
        isSortable: true,
        width: '22%',
      },
      {
        name: 'JORNADA',
        dataKey: 'jornadaLaboral',
        position: 'left',
        isSortable: true,
        width: '15%',
      },
      {
        name:
          this.helperService.jerarquiaSelected.regimen.codProg === '4'
            ? 'SUBVENCIÓN'
            : 'REMUNERACIÓN',
        dataKey: 'remuneracionConMoneda',
        position: 'left',
        isSortable: true,
        width: '10%',
      },
    ];
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Lista de vacantes';
    model.headers = [
      'PERFIL DE VACANTES',
      'CÓDIGO',
      'VACANTES',
      'LUGAR',
      'JORNADA',
      this.helperService.jerarquiaSelected.regimen.codProg === '4'
        ? 'SUBVENCIÓN'
        : 'REMUNERACIÓN',
    ];
    model.keys = [
      'perfilNombre',
      'codigoPuesto',
      'nroVacante',
      'sedeDireccion',
      'jornadaLaboral',
      'remuneracionConMoneda',
    ];
    return model;
  }
}
