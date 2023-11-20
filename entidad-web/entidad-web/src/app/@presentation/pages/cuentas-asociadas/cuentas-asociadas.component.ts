import { Component, OnInit, ViewChild } from '@angular/core';
import { RegistroEntidad } from '../../../@data/model/registro-entidad';
import {
  NbDialogService,
  NbSortDirection,
  NbSortRequest,
  NbTabsetComponent,
  NbToggleComponent,
  NbTreeGridDataSource,
  NbTreeGridDataSourceBuilder,
} from '@nebular/theme';
import { ParameterRepository } from '../../../@domain/repository/parameter.repository';
import { map } from 'rxjs/operators';
import { CuentaEntidadRepository } from '../../../@domain/repository/cuenta.entidad.repository';
import { TypeDocument } from '../../../@data/model/typeDocument';
import { FormBuilder, Validators } from '@angular/forms';
import { RoleUser } from '../../../@data/model/role';
import { ReniecRepository } from '../../../@domain/repository/reniec.repository';
import { ToastService } from '../../@common-components/toast';
import { Observable, of } from 'rxjs';
import { AdministratorRepository } from '../../../@domain/repository/administrator.repository';
import {
  CuentaEntidad,
  CuentaEntidadRequest,
  Rol,
} from '../../../@data/model/cuentaentidad';
import { Utils } from '../../../utils/utils';
import {
  ExportExcelModel,
  ExportExcelService,
} from '../../@service/export-excel.service';
import { ModalReasignarComponent } from './modal-reasignar/modal-reasignar.component';
import { MatDialog } from '@angular/material/dialog';
import { ModalRolesComponent } from './modal-roles/modal-roles.component';
import { PageEvent } from '@angular/material/paginator';
import { Country } from 'src/app/@data/model/country';
import { Const } from 'src/app/@data/services/const';
import { RegistroPostulante } from 'src/app/@data/model/registro.postulante';
import { AuthenticationRepository } from 'src/app/@domain/repository/authentication.repository';

@Component({
  selector: 'serv-talento-cuentas-asociadas',
  templateUrl: './cuentas-asociadas.component.html',
  styleUrls: ['./cuentas-asociadas.component.scss'],
})
export class CuentasAsociadasComponent implements OnInit {
  data: TreeNode<CuentaEntidad>[] = [];
  dataFiltered: TreeNode<CuentaEntidad>[] = [];
  data2: TreeNode<CuentaEntidad>[] = [];
  dataExport: ExportExcelModel;
  editNames: boolean = false;
  searchDocumentCompleate: boolean = false;
  length = 100;
  pageSize = 5;
  pageSizeOptions: number[] = [5, 10, 25, 100];
  settings = {
    noDataMessage: 'no existe información disponible',
    actions: {
      position: 'right',
      columnTitle: 'Acciones',
      delete: false,
    },
    mode: 'external',
    edit: {
      editButtonContent:
        '<i class="ent-web-edit" style="color : #43b02a !important; size: 1.5rem !important;"></i>',
      confirmSave: true,
    },
    // pager: {
    //   display: false,
    //   perPage: 10,
    // },
    hideSubHeader: true,
    columns: {
      nombreRol: {
        title: 'Rol',
        type: 'string',
      },
      nombres: {
        title: 'NOMBRES',
      },
      apellidos: {
        title: 'APELLIDOS',
      },
      descripcionPuesto: {
        title: 'PUESTO',
        type: 'string',
      },
      descripcionEstado: {
        title: 'ESTADO',
        type: 'string',
      },
      fechaBaja: {
        title: 'FECHA DE BAJA',
        type: 'string',
      },
    },
  };
  typeDocuments: TypeDocument[] = [];
  estadosRegistro: TypeDocument[] = [];
  roleslist: RoleUser[] = [];
  roleslistInitial: RoleUser[] = [];

  roleslistAll: RoleUser[] = [];

  roleslistReasing: RoleUser[] = [];
  roleslistSelect: RoleUser[] = [];
  roleslistSelectReasing: RoleUser[] = [];
  model: RegistroEntidad = new RegistroEntidad();
  cuentasForm = this.fb.group({
    typeDocument: ['', [Validators.required]],
    numberDocument: [
      '',
      [Validators.required, Validators.minLength(8), Validators.maxLength(12)],
    ],
    name: [
      '',
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    fatherName: [
      '',
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    motherName: [
      '',
      [
        Validators.required,
        Validators.pattern(/[A-Za-zÑñÁáÉéÍíÓóÚú ]$/),
        Validators.minLength(2),
        Validators.maxLength(30),
      ],
    ],
    email: [
      '',
      [
        Validators.required,
        Validators.pattern("^[a-z0-9._%+-]+@[a-z0-9.-]+\\.[a-z]{2,4}$"),
        Validators.minLength(10),
        Validators.maxLength(50),
      ],
    ],
    phone: [
      '',
      [
        Validators.required,
        Validators.pattern(/[0-9]$/),
        Validators.minLength(6),
        Validators.maxLength(9),
      ],
    ],
    anexo: ['', []],
    country: [''],
    personaId: [null],
    usuarioId: [null],
    correoId: [null],
    telefonoId: [null],
    cuentaId: [null],
    comment: [''],
    roles: [[]],
    puesto: [
      '',
      [Validators.required, Validators.minLength(3), Validators.maxLength(30)],
    ],
    stateCuentas: ['', [Validators.required]],
    rol: [null, [Validators.required]],
  });
  // rollistngmodel: any;
  isUpdate: boolean = false;
  isReasignable: boolean = false;
  dataUpdate: CuentaEntidad;

  const = Const;
  numeroDocumentoMaxlength: number = 8;
  numeroDocumentoType: string = 'integer';

  constructor(
    public fb: FormBuilder,
    private toastService: ToastService,
    private administratorRepository: AdministratorRepository,
    private cuentaEntidadServicio: CuentaEntidadRepository,
    private parametrosRepository: ParameterRepository,
    private reniecRepository: ReniecRepository,
    private exportExcelService: ExportExcelService,
    private dialogService: NbDialogService,
    private matDialog: MatDialog,
    public authenticationRepository: AuthenticationRepository,
    private dataSourceBuilder: NbTreeGridDataSourceBuilder<CuentaEntidad>
  ) {
    this.dataExport = this.getDataExport();
  }

  get f() {
    return this.cuentasForm.controls;
  }

  ngOnInit(): void {
    this.obtenerDataGrilla();
    this.parametrosRepository.getTypeDocuments().subscribe((response) => {
      this.typeDocuments = response;
    });
    this.parametrosRepository.getEstadoRegistro().subscribe((response) => {
      this.estadosRegistro = response;
    });
    this.parametrosRepository.getRolesCuentas().subscribe((response) => {
      this.roleslist = response;
      this.roleslistInitial = response;
      this.roleslist =  this.roleslist.filter((item) => item.rolId !== Const.R_ADMIN_ENTIDAD &&
      item.rolId !== Const.R_SUPER_ADMIN_ENTIDAD);
      console.log("this.roleslist:",this.roleslist)
      this.roleslistReasing = response;
      this.roleslistAll = response;
    });
    this.administratorRepository.getCountries().subscribe((response) => {
      this.countries = response;
      this.options = this.countries;
      this.filteredOptions$ = of(this.options);
    });

    this.enableNombres(false);
    this.enableEstado(false);
  }

  cambioRoles(values: number[]) {
    console.log("values:",values)

    this.roleslistSelect = this.roleslist.filter((item) =>
      values.includes(item.rolId)
    );
    console.log("roleslistSelect:",this.roleslistSelect)
    console.log("roleslist:",this.roleslist)

  }

  cambioRolesToogle(values: number[]) {
    this.roleslistSelectReasing = this.roleslistReasing.filter((item) =>
      values.includes(item.rolId)
    );
  }

  isDisabled(rolUsuario:any) {
   let retorno = false;
   if (this.isUpdate) {

    let rolesActivos = this.dataUpdate.listaRoles.filter((value) => +value.estadoId === 1);

    if ((rolesActivos[0].rolId === Const.R_ADMIN_ENTIDAD ||  rolesActivos[0].rolId === Const.R_SUPER_ADMIN_ENTIDAD)) {
        if (rolUsuario.rolId !==  Const.R_ADMIN_ENTIDAD  && rolUsuario.rolId  !== Const.R_SUPER_ADMIN_ENTIDAD)
        {
        retorno = true;
        }
    }
   
}
   return retorno;
  }

  verificarDocumento() {
    this.reniecRepository
      .getPersonInfo(this.f.numberDocument.value, this.f.typeDocument.value)
      .subscribe(
        (res) => {
          this.searchDocumentCompleate = true;
          this.f.typeDocument.disable();
          if (res === true) {
            this.enableNombres(true);
            this.toastService.showToast(
              'La persona con el CE ingresado no está registrada, ingrese los datos manualmente.',
              'warning'
            );
          } else {
            if (this.f.typeDocument.value === 4) {
              this.setCountryToID(res.paisId);
            }
            this.cuentasForm.patchValue({
              name: res.nombres,
              fatherName: res.apellidoPaterno,
              motherName: res.apellidoMaterno,
              personaId: res.personaId,
            });
            if (!this.isUpdate) {
              this.cuentasForm.patchValue({
                stateCuentas: this.getEstadoValue(1),
              });
            }
          }
        },
        (err) => {
          if (this.f.typeDocument.value !== 4) {
            this.toastService.showToast(err, 'danger');
          } else {
            this.toastService.showToast(
              'La persona con el CE ingresado no está registrada, ingrese los datos manualmente.',
              'danger'
            );
            this.searchDocumentCompleate = true;
            this.f.typeDocument.disable();
            this.enableNombres(true);
          }
        }
      );
  }

  enableNombres(value: boolean) {
    if (value) {
      this.f.name.enable();
      this.f.fatherName.enable();
      this.f.motherName.enable();
    } else {
      this.f.name.disable();
      this.f.fatherName.disable();
      this.f.motherName.disable();
    }
  }
  enableEstado(value: boolean) {
    if (value) {
      this.f.stateCuentas.enable();
    } else {
      this.f.stateCuentas.disable();
    }
  }

  clearSearch() {
    this.enableNombres(false);
    this.enableEstado(false);
    this.f.typeDocument.enable();
    this.f.typeDocument.reset('');
    this.f.typeDocument.updateValueAndValidity();
    this.f.numberDocument.enable();
    this.f.numberDocument.reset('');
    this.f.rol.reset([]);
    this.f.numberDocument.updateValueAndValidity();
    this.f.name.updateValueAndValidity();
    this.f.name.reset('');
    this.f.fatherName.updateValueAndValidity();
    this.f.fatherName.reset('');
    this.f.motherName.updateValueAndValidity();
    this.f.motherName.reset('');
    this.f.country.enable();
    this.searchDocumentCompleate = false;
  }

  // ---------------------------------------------- //
  // --- Autocomplete for countries ---- //
  // ----------------------------------------------- //
  options: Country[];
  filteredOptions$: Observable<Country[]>;
  countries: Country[];

  @ViewChild('inputCountry') input;
  @ViewChild('inputCountry2') input2;
  @ViewChild('tabs') tabs: NbTabsetComponent;

  private filter(value: string): Country[] {
    const filterValue = value?.toLowerCase();
    return this.options?.filter((optionValue) =>
      optionValue.nombrePais.toLowerCase().includes(filterValue)
    );
  }

  getFilteredOptions(value: string): Observable<Country[]> {
    return of(value).pipe(map((filterString) => this.filter(filterString)));
  }

  onChange() {
    this.verifyCountry();
    this.filteredOptions$ = this.getFilteredOptions(
      !this.isReasignable
        ? this.input.nativeElement.value
        : this.input2.nativeElement.value
    );
  }

  onSelectionChange($event) {
    // this.setCountry($event);
    this.filteredOptions$ = this.getFilteredOptions($event);
  }

  setCountryToID(idPais: number) {
    this.cuentasForm
      .get('country')
      .setValue(this.countries?.find((o) => o.paisId === idPais).nombrePais);
    this.f.country.disable();
    this.verifyCountry();
    this.input.nativeElement.disabled = true;
    this.input2.nativeElement.disabled = true;
  }

  verifyCountry() {
    const actualValue = this.cuentasForm.get('country').value;
    if (!this.countries?.filter((o) => o.nombrePais === actualValue)[0]) {
      this.cuentasForm.get('country').setErrors({ notfound: true });
    } else {
      // this.setCountry(actualValue);
    }
  }

  changeTypeDocument() {
    switch (this.f.typeDocument.value) {
      case 1:
        this.cuentasForm.get('country').clearValidators();
        this.cuentasForm.get('motherName').setValidators(Validators.required);
        this.cuentasForm.get('country').updateValueAndValidity();
        this.cuentasForm.get('motherName').updateValueAndValidity();
        this.cuentasForm.get('numberDocument').clearValidators();
        this.cuentasForm.get('numberDocument').setValue('');
        this.cuentasForm
          .get('numberDocument')
          .setValidators([
            Validators.required,
            Validators.pattern(/^[0-9]{8}$/),
            Validators.maxLength(8)
          ]);
        this.numeroDocumentoMaxlength = 8;
        this.numeroDocumentoType = 'integer';
        this.cuentasForm.get('numberDocument').updateValueAndValidity();
        break;
      case 4:
        this.cuentasForm.get('country').setValidators(Validators.required);
        this.cuentasForm.get('motherName').clearValidators();
        this.cuentasForm.get('country').updateValueAndValidity();
        this.cuentasForm.get('motherName').updateValueAndValidity();
        this.cuentasForm.get('numberDocument').clearValidators();
        this.cuentasForm.get('numberDocument').setValue('');
        this.cuentasForm
          .get('numberDocument')
          .setValidators([
            Validators.required,
            Validators.pattern(/^[a-zA-Z0-9]{9,12}$/),
            Validators.maxLength(12)
          ]);
        this.numeroDocumentoMaxlength = 12;
        this.numeroDocumentoType = 'noSpecialChars';
        this.cuentasForm.get('numberDocument').updateValueAndValidity();
        break;
      default:
        break;
    }
  }

  validEnableSearchButtom() {
    return this.f.numberDocument.valid && this.f.typeDocument.errors == null;
  }

  getDataSend(reasignar: boolean): CuentaEntidadRequest {
    let result = new CuentaEntidadRequest();
    result.typeDocument = this.f.typeDocument.value;
    result.numberDocument = this.f.numberDocument.value;
    result.name = this.f.name.value;
    result.fatherName = this.f.fatherName.value;
    result.motherName =
      this.f.motherName.value === null ? '' : this.f.motherName.value;
    result.email = this.f.email.value;
    result.phone = this.f.phone.value;
    result.anexo = this.f.anexo.value;
    result.country =
      this.f.typeDocument.value !== 4
        ? null
        : this.countries.find(
            (item) => item.nombrePais === this.f.country.value
          ).paisId;
    result.puesto = this.f.puesto.value;
    result.stateCuentas = this.f.stateCuentas.value;
    result.usuarioId = this.f.usuarioId.value;
    // result.rol = this.f.rol.value;
    result.roles = this.f.roles.value != null ? this.f.roles.value.filter(item => this.f.rol.value.includes(item.rolId)) : this.f.roles.value;
    let roles = null;
    if ( result.roles == null ) {
      roles = null;
    } else  {
      if (this.f.rol.value.length > result.roles.length) {
        roles = this.f.rol.value.map((value) => {
          return { usuarioId: result.usuarioId, rolId: value, usuarioRolId: null };
        });
        result.roles.forEach(r => {
          roles.forEach(it => {
            if ( it.usuarioRolId == null && r.rolId === it.rolId) {
              it.usuarioRolId = r.usuarioRolId;
            }
          });
        });
      } else {
        roles = result.roles.map((value) => {
          return { usuarioId: result.usuarioId, rolId: value.rolId, usuarioRolId: value.usuarioRolId };
        });
      }
    }
    result.rol = roles ? roles : this.f.rol.value;
    if (reasignar) {
      // result.roles = this.f.roles.value.filter(item => this.f.rol.value.includes(item.rolId));
      result.commet = this.f.comment.value;
    }
    result.personaId = this.f.personaId.value;
    result.correoId = this.f.correoId.value;
    result.telefonoId = this.f.telefonoId.value;
    result.cuentaId = this.f.cuentaId.value;
    return result;
  }

  createCuenta() {

    this.cuentaEntidadServicio.saveRegistro(this.getDataSend(false)).subscribe(
      (value: any) => {
        this.limpiar();
        if( value.status.success ) {
          this.toastService.showToast( 'Cuenta asociada creada correctamente','success');
        } else {
          this.toastService.showToast( value.status.error.messages[0],'danger');
        }
        this.obtenerDataGrilla();
        this.ngOnInit();
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  updateCuenta() {

    let roles = this.f.roles.value != null ? this.f.roles.value.filter(item => this.f.rol.value.includes(item.rolId)) : this.f.roles.value;
    let rolesActivos = roles.filter((value) => value.rolId === Const.R_SUPER_ADMIN_ENTIDAD);

    this.cuentaEntidadServicio.updateCuenta(this.getDataSend(false)).subscribe(
      (value: any) => {
        this.limpiar();
        if (value.status.success) {
          this.toastService.showToast(
            'Cuenta asociada actualizada correctamente',
            'success'
          ); 
          if (rolesActivos !== null && rolesActivos.length > 0 && rolesActivos[0].rolId === Const.R_SUPER_ADMIN_ENTIDAD) {
            this.registrarPostulante();
          } else {
          this.obtenerDataGrilla();
          this.ngOnInit();
          }
        } else {
          this.toastService.showToast(value.status.error.messages[0],'danger');
          this.obtenerDataGrilla();
          this.ngOnInit();
        }
       
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  registrarPostulante() {
    const userId = sessionStorage.getItem('userId');
    const persona = JSON.parse (sessionStorage.getItem ("persona"));
    
    let data: RegistroPostulante = new RegistroPostulante({
      usuarioId: Number(userId),
	    personaId: persona.personaId
    });
     
    this.administratorRepository.registerPostulante(data).subscribe(
      (value) => {
        this.toastService.showToast(
          'Registrado correctamente, espera las pasos a seguir en su correo (No olvide revisar su correo SPAM)',
          'success'
        );
        this.obtenerDataGrilla();
        this.ngOnInit();
      },
      (error) => {
        this.obtenerDataGrilla();
        this.ngOnInit();
      }
    );
  }

  obtenerDataGrilla() {
    this.limpiar();
    this.isReasignable = false;
    this.cuentaEntidadServicio.getCuentasEntidad().subscribe((response) => {
      this.data = [];
      response.forEach((item) => {
        let itemData: TreeNode<CuentaEntidad> = new TreeNode<CuentaEntidad>();
        itemData.data = item;
        itemData.children = item.listaRoles.map((itemRol) => {
          let itemDataRol: TreeNode<CuentaEntidad> = new TreeNode<CuentaEntidad>();

          let cuentaRol: CuentaEntidad = new CuentaEntidad();
          cuentaRol.rol = itemRol.nombreRol;
          cuentaRol.fechaAlta = Utils.parseDate(
            itemRol.fechaAltaRol,
            'DD/MM/yyyy'
          );
          cuentaRol.fechaBaja = Utils.parseDate(
            itemRol.fechaBajaRol,
            'DD/MM/yyyy'
          );
          cuentaRol.descripcionEstado = itemRol.descripcionEstado;
          cuentaRol.estadoId = itemRol.estadoId;
          cuentaRol.rolId = itemRol.rolId;
          cuentaRol.editable = itemRol.editable;
          cuentaRol.listaRoles.push(itemRol);

          itemDataRol.data = cuentaRol;
          itemDataRol.expanded = false;
          return itemDataRol;
        });
        itemData.expanded = false;
        this.data.push(itemData);
      });
      this.data.forEach((item) => {
        item.data.rolId =
          item.data.listaRoles.find(
            (element) => element.rolId === Const.R_ADMIN_ENTIDAD
          ) === undefined
            ? 0
            : Const.R_ADMIN_ENTIDAD;
      });
      this.length = this.data.length;
      this.dataFiltered = this.data.slice(0);
      const aux = this.data.slice(0);
      this.data2 = aux.splice(0, this.pageSize);
      this.dataSource = this.dataSourceBuilder.create(this.data2);
      this.dataSourceItem = this.dataSourceBuilder.create(this.data);
    });
  }

  getDataEvent(e: PageEvent) {
    this.pageSize = e.pageSize;
    const aux = this.dataFiltered.slice(0);
    this.data2 = aux.splice(e.pageIndex * e.pageSize, this.pageSize);
    this.dataSource = this.dataSourceBuilder.create(this.data2);
  }

  buscarCuentas(e) {
    const query = e.target.value.toLowerCase();
    if (query.length > 0) {
      let arreglo = this.data.filter((d, index) => {
        const roles = d.data.listaRoles.map((r) => r.nombreRol);
        return (
          d.data.nombreCompleto.toLowerCase().includes(query) ||
          d.data.descripcionPuesto.toLowerCase().includes(query) ||
          d.data.descripcionEstado.toLowerCase().includes(query) ||
          (roles
            .map((r) => (r.toLowerCase().includes(query) ? true : false))
            .includes(true)
            ? true
            : false )
        );
      });
      this.length = arreglo.length;
      this.dataFiltered = arreglo.slice(0);
      const aux2 = this.dataFiltered.slice(0);
      this.data2 = aux2.splice(0, this.pageSize);
      this.dataSource = this.dataSourceBuilder.create(this.data2);
      this.dataSourceItem = this.dataSourceBuilder.create(this.data);
      console.info(this.data);
    } else {
      this.length = this.data.length;
      const aux = this.data.slice(0);
      this.dataFiltered = aux;
      this.dataSource = this.dataSourceBuilder.create(this.data2);
      this.dataSourceItem = this.dataSourceBuilder.create(this.data);
      console.info(this.data);
    }
  }

  customColumn = 'nombreCompleto';
  defaultColumns = [
    'descripcionPuesto',
    'rol',
    'fechaAlta',
    'fechaBaja',
    'descripcionEstado',
    'acciones',
  ];
  nameHeaders = {
    nombreCompleto: 'Nombre Completo',
    descripcionPuesto: 'Puesto',
    rol: 'Rol',
    fechaAlta: 'Fecha Alta',
    fechaBaja: 'Fecha Baja',
    descripcionEstado: 'Estado',
    acciones: 'Acciones',
  };
  allColumns = [this.customColumn, ...this.defaultColumns];
  dataSource: NbTreeGridDataSource<CuentaEntidad>;
  dataSourceItem: NbTreeGridDataSource<CuentaEntidad>;
  sortColumn: string;
  sortDirection: NbSortDirection = NbSortDirection.NONE;

  updateSort(sortRequest: NbSortRequest): void {
    this.sortColumn = sortRequest.column;
    this.sortDirection = sortRequest.direction;
    this.dataSourceItem.sort(sortRequest);
  }

  getSortDirection(column: string): NbSortDirection {
    if (this.sortColumn === column) {
      return this.sortDirection;
    }
    return NbSortDirection.NONE;
  }
  getShowOn(index: number) {
    const minWithForMultipleColumns = 400;
    const nextColumnStep = 100;
    return minWithForMultipleColumns + nextColumnStep * index;
  }

  editData(row: TreeNode<CuentaEntidad>) {
    let rolId = 0;
    this.roleslist = this.roleslist.filter((item) => item.rolId !== Const.R_ADMIN_ENTIDAD &&
    item.rolId !== Const.R_SUPER_ADMIN_ENTIDAD);

    let rolesActivos = row.data.listaRoles.filter((value) => +value.estadoId === 1);

    if (rolesActivos!== null && rolesActivos.length>0) {
        rolId =rolesActivos[0].rolId;
    }

  if ( rolId === Const.R_ADMIN_ENTIDAD || rolId === Const.R_SUPER_ADMIN_ENTIDAD ) {
    this.roleslist = this.roleslistAll;
  }

    setTimeout(() => {
    console.log(" editandoooooo:")
      this.isUpdate = true;
      this.setData(row.data);
      this.f.numberDocument.disable();
  }, 0);

  }
 
  setData(data: CuentaEntidad) {
    this.limpiar();
    let rolesActivos = data.listaRoles.filter((value) => +value.estadoId === 1);
    this.cuentasForm.patchValue({
      typeDocument: data.tipoDocumento,
      numberDocument: data.nroDocumento,
      email: data.correo,
      phone: data.numeroTelefono,
      anexo: data.anexo,
      country: this.getNombrePais(data.paisId),
      puesto: data.descripcionPuesto,
      correoId: data.correoId,
      cuentaId: data.cuentaId,
      telefonoId: data.telefonoId,
      roles: data.listaRoles,
      usuarioId: data.usuarioId,
      stateCuentas: this.getEstadoValue(+data.estadoId),
      rol:
        rolesActivos != null && rolesActivos.length !== 0
          ? rolesActivos.map((value) => value.rolId)
          : [],
    });
    this.dataUpdate = data;
    this.enableEstado(true);
    console.log("rolesActivos:",rolesActivos)
    console.log("dataUpdate:",this.dataUpdate)

    this.cambioRoles(rolesActivos.map((value) => value.rolId));
    this.verificarDocumento();
  }

  limpiarTodo() {
    this.roleslist = this.roleslist.filter((item) => item.rolId !== Const.R_ADMIN_ENTIDAD &&
    item.rolId !== Const.R_SUPER_ADMIN_ENTIDAD);
    this.f.numberDocument.enable();
    this.isUpdate = false;
    this.limpiar();
  }

  limpiar() {
    this.cuentasForm.get('comment').clearValidators();
    this.cuentasForm.get('comment').updateValueAndValidity();
    this.roleslistSelect = [];
    this.f.rol.reset(['']);
    try {
      this.cuentasForm.reset('');
    } catch (e) {}
    try {
      this.input.nativeElement.disabled = false;
      this.input2.nativeElement.disabled = false;
    } catch (e) {}
    this.clearSearch();
  }

  cambiarEstado(cuentaId: number, active: any) {
    this.validCuentaForInactive(
      cuentaId,
      +this.estadosRegistro.find((value) => value.parametroId === active)
        .codigoNumero === 1
    );
  }
  cambiarEstadoToogle(
    cuenta: CuentaEntidad,
    active: any,
    toggle?: NbToggleComponent,
    isrol?: boolean,
    roldata?: Rol
  ) {
    this.validCuentaForInactiveToogle(
      cuenta,
      toggle != null
        ? active
        : +this.estadosRegistro.find((value) => value.parametroId === active)
            .codigoNumero === 1,
      toggle,
      isrol,
      roldata
    );
  }

  exportData() {
    let newData: CuentaEntidad[] = [];
    this.data.forEach((value) => {
      if (value != null) {
        value.data.tipoDocumentoTexto = this.typeDocuments.find(
          (typeDocu) => value.data.tipoDocumento === +typeDocu.codigoNumero
        ).valorTexto;
        value.data.pais = this.getNombrePais(value.data.paisId);
        newData.push(value.data);
        if (value.children != null && value.children.length !== 0) {
          newData = newData.concat(value.children.map((item) => item.data));
        }
      }
    });
    this.dataExport.data = newData;
    this.exportExcelService.exportExcel(this.dataExport);
  }

  getDataExport(): ExportExcelModel {
    let model = new ExportExcelModel();
    model.title = 'Cuentas Asociadas';
    model.headers = [
      'Tipo de documento',
      'N° de documento',
      'Nombres',
      'Apellido paterno',
      'Apellido materno',
      'País',
      'Puesto',
      'Correo laboral',
      'Teléfono',
      'Anexo',
      'Roles',
      'Fecha Alta',
      'Fecha Baja',
      'Estado',
    ];
    model.keys = [
      'tipoDocumentoTexto',
      'nroDocumento',
      'nombres',
      'apellidoPaterno',
      'apellidoMaterno',
      'pais',
      'descripcionPuesto',
      'correo',
      'numeroTelefono',
      'anexo',
      'rol',
      'fechaAlta',
      'fechaBaja',
      'descripcionEstado',
    ];
    return model;
  }

  getEstadoValue(getId: number) {
    return this.estadosRegistro.find((value) => +value.codigoNumero === getId)
      .parametroId;
  }

  private getNombrePais(paisId: number) {
    return paisId != null
      ? this.countries
          .find((country) => country.paisId === paisId)
          .nombrePais.toUpperCase()
      : null;
  }

  validCuentaForInactive(cuentaId: number, active: boolean) {
    if (!active) {
      this.cuentaEntidadServicio.validCuenta(cuentaId).subscribe(
        (eliminable) => {
          if (!eliminable) {
            this.enableEstado(true);
            this.modalReasignable();
          }
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
    }
  }
  validCuentaForInactiveToogle(
    cuenta: CuentaEntidad,
    active: boolean,
    toggle?: NbToggleComponent,
    isrol?: boolean,
    roldata?: Rol
  ) {
    if (!isrol) {
      this.cambiarEstadoCuentaServicio(cuenta.cuentaId, active);
    } else {
      this.cuentaEntidadServicio
        .updateUsuarioRol(
          roldata.usuarioRolId,
          active,
          Utils.getfechaFormatoEnvio(roldata.fechaAltaRol)
        )
        .subscribe((result) => {
          if (result) {
            this.toastService.showToast(
              `Rol ${active ? 'activado' : 'desactivado'} correctamente`,
              'success'
            );
            this.obtenerDataGrilla();
            this.isUpdate = false;
          } else {
            this.toastService.showToast(
              `Error al  ${active ? 'activar' : 'desactivar'} rol`,
              'danger'
            );
            toggle.checked = !active;
          }
        });
    }
  }

  modalReasignable() {
    this.dialogService
      .open(ModalReasignarComponent, {
        context: {},
        closeOnBackdropClick: false,
        autoFocus: false,
        closeOnEsc: false,
      })
      .onClose.subscribe((value: boolean) => {
        if (value) {
          this.limpiarTodo();
          this.isReasignable = true;
          this.cuentasForm.get('comment').setValidators([Validators.required]);
          this.cuentasForm.get('comment').updateValueAndValidity();
          this.roleslistReasing = this.dataUpdate.listaRoles
            .filter(
              (item) =>
                item.rolId !== Const.R_ADMIN_ENTIDAD &&
                item.rolId !== Const.R_ADMIN_SERVIR
            )
            .map((val) => new RoleUser(val.rolId, val.nombreRol));
          this.roleslistSelectReasing = this.dataUpdate.listaRoles
            .filter(
              (item) =>
                item.rolId !== Const.R_ADMIN_ENTIDAD &&
                item.rolId !== Const.R_ADMIN_SERVIR
            )
            .map(
              (valRolReasigned) =>
                new RoleUser(valRolReasigned.rolId, valRolReasigned.nombreRol)
            );
          this.cuentasForm.patchValue({
            cuentaId: this.dataUpdate.cuentaId,
            roles: this.roleslistSelectReasing.filter(
              (item) =>
                item.rolId !== Const.R_ADMIN_ENTIDAD &&
                item.rolId !== Const.R_ADMIN_SERVIR
            ),
          });

          let rolesActivos = this.dataUpdate.listaRoles.filter(
            (rol) => +rol.estadoId === 1
          );
          this.cuentasForm.patchValue({
            rol:
              rolesActivos != null && rolesActivos.length !== 0
                ? rolesActivos.map((ra) => ra.rolId)
                : [],
          });
          this.cambioRolesToogle(rolesActivos.map((ractivo) => ractivo.rolId));
          // this.cambioRoles(rolesActivos.map(value => value.rolId));
        } else {
          this.f.stateCuentas.reset(this.getEstadoValue(1));
        }
      });
  }

  modalReasignableToogle(cuenta: CuentaEntidad) {
    this.dialogService
      .open(ModalReasignarComponent, {
        context: {},
        closeOnBackdropClick: false,
        autoFocus: false,
        closeOnEsc: false,
      })
      .onClose.subscribe((value: boolean) => {
        if (value) {
          this.limpiarTodo();
          this.isReasignable = true;
          this.cuentasForm.get('comment').setValidators([Validators.required]);
          this.cuentasForm.get('comment').updateValueAndValidity();
          this.roleslistSelectReasing = cuenta.listaRoles
            .filter(
              (item) =>
                item.rolId !== Const.R_ADMIN_ENTIDAD &&
                item.rolId !== Const.R_ADMIN_SERVIR
            )
            .map(
              (roleSelectReasing) =>
                new RoleUser(
                  roleSelectReasing.rolId,
                  roleSelectReasing.nombreRol
                )
            );
          this.roleslistReasing = cuenta.listaRoles
            .filter(
              (item) =>
                item.rolId !== Const.R_ADMIN_ENTIDAD &&
                item.rolId !== Const.R_ADMIN_SERVIR
            )
            .map(
              (rolListReasign) =>
                new RoleUser(rolListReasign.rolId, rolListReasign.nombreRol)
            );
          this.cuentasForm.patchValue({
            cuentaId: cuenta.cuentaId,
            usuarioId: cuenta.usuarioId,
            roles: cuenta.listaRoles.filter(
              (item) =>
                item.rolId !== Const.R_ADMIN_ENTIDAD &&
                item.rolId !== Const.R_ADMIN_SERVIR
            ),
            rol: cuenta.listaRoles
              .filter(
                (item) =>
                  item.rolId !== Const.R_ADMIN_ENTIDAD &&
                  item.rolId !== Const.R_ADMIN_SERVIR
              )
              .map((rol) => rol.rolId),
          });
          this.f.rol.updateValueAndValidity();
          this.cambioRolesToogle(this.f.rol.value);
        }
      });
  }

  cambiarEstadoCuentaServicio(cuentaId: number, active: boolean) {
    this.cuentaEntidadServicio.deleteCuenta(cuentaId, active ? 1 : 0).subscribe(
      (value) => {
        this.toastService.showToast(
          `Cuenta asociada ${active ? 'activada' : 'desactivada'}`,
          'success'
        );
        this.obtenerDataGrilla();
        this.isUpdate = false;
      },
      (err) => {
        this.toastService.showToast(err, 'danger');
      }
    );
  }

  reasignarCuenta() {
    this.cuentaEntidadServicio
      .reasignarCtaAsociada(this.getDataSend(true), +this.f.cuentaId.value)
      .subscribe(
        (value) => {
          this.toastService.showToast(
            'Cuenta asociada reasignada correctamente',
            'success'
          );
          this.obtenerDataGrilla();
        },
        (err) => {
          this.toastService.showToast(err, 'danger');
        }
      );
  }

  openRoleModal() {
    this.matDialog.open(ModalRolesComponent, {
      data: null,
      width: '50vw',
      height: '80vh',
      maxWidth: '56.25rem',
    });
  }
}

class TreeNode<T> {
  data: T;
  children?: TreeNode<T>[];
  expanded?: boolean;
}
