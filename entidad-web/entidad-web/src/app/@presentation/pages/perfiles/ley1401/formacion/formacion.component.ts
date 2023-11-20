import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { MaestraRepository } from '../../../../../@domain/repository/maestra.reposity';
import { ModalNivelEducatvoComponent } from '../../components/modal-nivel-educatvo/modal-nivel-educatvo.component';
import { HelperLey1401Service } from '../helperLey1401.service';

@Component({
  selector: 'serv-talento-formacion',
  templateUrl: './formacion.component.html',
  styleUrls: ['./formacion.component.scss'],
})
export class FormacionComponent implements OnInit {
  arrayFormaciones: any[] = [];
  minConocimientos = 1;
  deletedItems = [];
  minItems: number = 0;
  maxItems: number = 10;

  conocimientoMap: Map<number, any[]> = new Map<number, any[]>();
  listaoriginal: any[] = [];

  constructor(
    public helper1401Service: HelperLey1401Service,
    public maestraRepository: MaestraRepository,
    private dialog: MatDialog
  ) {}

  get f() {
    return this.helper1401Service.formacionAcademicaForm.controls;
  }

  ngOnInit(): void {
    this.conocimientoMap = this.helper1401Service.conocimientoMap;
    this.listaoriginal = this.helper1401Service.listaoriginal;

    this.helper1401Service.formacionAcademicaForm
      .get('nivelesAcademicos')
      .valueChanges.subscribe(() => {
        this.arrayFormaciones = this.helper1401Service.formacionAcademicaForm.get(
          'nivelesAcademicos'
        ).value;
      });
  }

  openModal(data?: any) {
    const modalNivelEducativo = this.dialog.open(ModalNivelEducatvoComponent, {
      data,
      width: '56rem',
    });
    modalNivelEducativo.afterClosed().subscribe((res) => {
      if (res) {
        this.helper1401Service.formacionAcademicaForm.markAsDirty();
        if (!res.orden && res.orden !== 0) {
          this.arrayFormaciones.push(res);
          this.arrayFormaciones.forEach((el, index) => {
            el.orden = index;
          });
          this.arrayFormaciones = [...this.arrayFormaciones];
          this.f.nivelesAcademicos.patchValue(this.arrayFormaciones);
        } else {
          this.arrayFormaciones[res.orden] = {
            ...this.arrayFormaciones[res.orden],
            nivel: res.nivel,
            grado: res.grado,
            tipoGrado: res.tipoGrado,
            nombreGrado: res.nombreGrado,
            carreras: res.carreras,
            tipoNivel: res.tipoNivel,
            estado: res.estado,
            formacionAcademicaId: res.formacionAcademicaId,
            carrerasToDelete: res.carrerasToDelete,
          };
          this.arrayFormaciones = [...this.arrayFormaciones];
          this.f.nivelesAcademicos.patchValue(this.arrayFormaciones);
        }
      }
    });
  }

  deleteItem(event: any) {
    const order = this.arrayFormaciones.indexOf (event);
    this.helper1401Service.formacionAcademicaForm
      .get('nivelesAcademicosToDelete')
      .patchValue([
        ...this.helper1401Service.formacionAcademicaForm.get(
          'nivelesAcademicosToDelete'
        ).value,
        this.arrayFormaciones[order],
      ]);
    this.arrayFormaciones.splice(order, 1);
    this.arrayFormaciones.forEach((el, index) => {
      el.orden = index;
    });
    this.arrayFormaciones = [...this.arrayFormaciones];
  }

  addNewItem() {
    const actualValue = this.f.conocimientosBasicos.value.slice(0);
    actualValue.push({
      funcionDetalleId: null,
      extra: '',
      descripcion: '',
      estado: 1,
    });
    this.f.conocimientosBasicos.patchValue(actualValue);
    this.f.conocimientosBasicos.markAsDirty();
  }

  removeItem(index: number) {
    const array = this.f.conocimientosBasicos.value.slice(0);
    if (array[index].funcionDetalleId) {
      array[index].estado = 0;
      this.helper1401Service.formacionDeletedItems.push(array[index]);
    }
    array.splice(index, 1);
    this.f.conocimientosBasicos.patchValue(array);
    this.f.conocimientosBasicos.markAsDirty();
  }

  itemsToShow() {
    return this.f.conocimientosBasicos.value;
  }
}
