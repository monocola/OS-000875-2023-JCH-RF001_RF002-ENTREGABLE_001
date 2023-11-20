import { Component, Input, OnInit } from '@angular/core';
import { NbDialogRef } from '@nebular/theme';
import { FormBuilder, FormControl, FormGroup, Validators } from '@angular/forms';
import { UsuarioGdr } from 'src/app/@data/model/usuarioGdr';
import { ComboItem } from 'src/app/@data/model/comboItem';
import { HttpEventType, HttpResponse } from '@angular/common/http';
import { ToastService } from 'src/app/@presentation/@common-components/toast';
import { ImplementacionRepository } from 'src/app/@domain/repository/implementacion.repository';

@Component({
  selector: 'serv-talento-detalle.gestor.gdr',
  templateUrl: './detalle-gestor-gdr.component.html',
  styleUrls: ['./detalle-gestor-gdr.component.scss']
})
export class DetalleGestorGdrComponent implements OnInit {

  @Input()
  entidadId: number = null;
  @Input()
  idGestor: number = null;
  @Input()
  isEdit: boolean = false;
  showResult: boolean = false;
  frmSuperior: FormGroup;
  formSearch: FormGroup;
  detailGestor: UsuarioGdr;
  unidadesOrganicas: ComboItem[] = [];
  puestos: ComboItem[] = [];
  typeDocuments: ComboItem[] = [];
  result: any;
  uuid: any;
  conteoupload: number = 0;
 
  constructor(
    protected ref: NbDialogRef<DetalleGestorGdrComponent>,
    private fb: FormBuilder,
    private toastService: ToastService,
    private implementacionRepository: ImplementacionRepository,
  ) {
    this.formSearch = this.fb.group({
      typedocument: new FormControl(null, Validators.required),
      nrodocument: new FormControl(null, Validators.required),
    });
    this.frmSuperior = this.fb.group({
      nombreCompleto: new FormControl(null, Validators.required),
      correo: new FormControl(null, Validators.required),
      unidadOrganica: new FormControl(null, Validators.required),
      puesto: new FormControl(null, Validators.required),
      estado: new FormControl(null, Validators.required),
    });

  }

  async ngOnInit() {
    if (this.isEdit) {
      this.showResult = true;
      if (this.idGestor != null) {
        this.detailGestor = await this.implementacionRepository.getDetailGestorGDR(this.idGestor).toPromise();
        this.uuid = this.detailGestor.uuId;
        if (this.detailGestor.nombreArchivo != null)
          this.file = new File([], this.detailGestor.nombreArchivo);
        this.unidadesOrganicas.push({
          value: this.detailGestor.unidadOrganicaId,
          description: this.detailGestor.unidadOrganica,
        });
        this.puestos.push({
          value: this.detailGestor.puestoId,
          description: this.detailGestor.puesto,
        });
        this.frmSuperior.patchValue({
          nombreCompleto: this.detailGestor.nombreCompleto,
          correo: this.detailGestor.correo,
          estado: this.detailGestor.estadovalue,
          unidadOrganica: this.detailGestor.unidadOrganicaId,
          puesto: this.detailGestor.puestoId,
        });
        this.frmSuperior.controls.nombreCompleto.disable();
        this.frmSuperior.controls.correo.disable();
        this.frmSuperior.controls.unidadOrganica.disable();
        this.frmSuperior.controls.puesto.disable();
      }
    } else {
      this.typeDocuments = await this.implementacionRepository.getTypeDocuments().toPromise();
      console.info(this.typeDocuments);
    }
  }

  dismiss(success: boolean) {
    this.ref.close(success);
  }

  async search() {
    try {
      this.result = await this.implementacionRepository.getServidoresCiviles(this.entidadId, this.formSearch.controls.typedocument.value, this.formSearch.controls.nrodocument.value).toPromise();
      this.frmSuperior.controls.puesto.disable();
      this.frmSuperior.controls.estado.clearAsyncValidators();
      this.unidadesOrganicas = this.result.listaUO.map(item => {
        return {
          value: item.detalleUOId,
          description: item.siglaUO,
          data: item.listaPuestos.map(itempuesto => {
            return {value: itempuesto.puestoId, description: itempuesto.descPuesto};
          }),
        };
      });
      this.frmSuperior.controls.unidadOrganica.valueChanges.subscribe(newvalue => {
        this.frmSuperior.controls.puesto.reset();
        this.frmSuperior.controls.puesto.enable();
        this.puestos = this.unidadesOrganicas.find(item => item.value === newvalue).data;
      });
      this.showResult = true;
      this.frmSuperior.patchValue({
        nombreCompleto: this.result.nombresApellidos,
        correo: this.result.correoPrincipal,
      });

      this.frmSuperior.controls.nombreCompleto.disable();
      this.frmSuperior.controls.correo.disable();
    } catch (e) {
      this.toastService.showToast(e, 'danger');
    }
  }

  async grabar() {
    try {
      if (this.isEdit) {
        let data = {
          gestorEntidadId: this.idGestor,
          estado: this.frmSuperior.controls.estado.value,
          nombreArchivo: this.file.name !== null ? this.file.name : this.detailGestor.nombreArchivo,
          uuidId: this.detailGestor.uuId,
        };
        this.implementacionRepository.updateGestor(data, this.file)
          .subscribe( (response: any) => {
            if (response.type === HttpEventType.UploadProgress) {
              this.conteoupload = Math.round(
                (100 * response.loaded) / response.total
              );
            } else if (response instanceof HttpResponse) {
              if ( !response.body.status.success ) {
                this.toastService.showToast(response.body.status.error.messages[0], 'danger');
              } else {
                this.toastService.showToast('Se actualizó el gestor con Exito', 'success');
                this.dismiss(true);
              }
            }
        });

      } else {
        let data = {
          entidadId: this.entidadId,
          personaId: this.result.personaId,
          nroDocumento: this.result.nroDocumento,
          detalleUoId: this.frmSuperior.controls.unidadOrganica.value,
          unidadOrganicaId: this.frmSuperior.controls.unidadOrganica.value,
          unidadOrganica: this.unidadesOrganicas.find(item => item.value === this.frmSuperior.controls.unidadOrganica.value).description,
          puesto: this.puestos.find(item => item.value === this.frmSuperior.controls.puesto.value).description,
          puestoId: this.frmSuperior.controls.puesto.value,
          file: this.file.size !== 0 ? this.file : null,
          nombres: this.result.nombres,
          apellidoPaterno: this.result.apellidoPaterno,
          apellidoMaterno: this.result.apellidoMaterno,
        };
        let result = await this.implementacionRepository.newGestor(data).toPromise();
        if (result.status.success) {
          this.toastService.showToast('Se registro el gestor con Exito', 'success');
          this.dismiss(true);
        }
      }
    } catch (err) {
      console.log(err);
      this.toastService.showToast(err, 'danger');
    }

  }

  file: File;
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

  validateDisable() {
    return this.frmSuperior.controls.nombreCompleto.invalid ||
      this.frmSuperior.controls.correo.invalid ||
      this.frmSuperior.controls.unidadOrganica.invalid ||
      this.frmSuperior.controls.puesto.invalid ||
      this.file == null;
  }
}
