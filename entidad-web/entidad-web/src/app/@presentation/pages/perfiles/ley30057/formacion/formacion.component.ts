import { Component, Input, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { ModalNivelEducatvoComponent } from '../../components/modal-nivel-educatvo/modal-nivel-educatvo.component';
import { MaestraRepository } from '../../../../../@domain/repository/maestra.reposity';
import { Const } from '../../../../../@data/services/const';
import { Autocompleateagrupado } from './autocompleateagrupado/autocompleateagrupado.component';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';

@Component({
  selector: 'serv-talento-formacion',
  templateUrl: './formacion.component.html',
  styleUrls: ['./formacion.component.scss'],
})
export class FormacionComponent implements OnInit {
  @Input() helper30057Service: any;

  arrayFormaciones: any[] = [];
  listaConocimientoTecnico: Autocompleateagrupado = null;
  listaCursosEsp: Autocompleateagrupado = null;
  listaProgramEsp: Autocompleateagrupado = null;
  listaConOfi: Autocompleateagrupado = null;
  listaConIdio: Autocompleateagrupado = null;

  conocimientoTableColumns: TableColumn[];

  constructor(
    private dialog: MatDialog,
    private maestraRepository: MaestraRepository
  ) {}

  get f() {
    return this.helper30057Service.formacionAcademicaForm.controls;
  }

  ngOnInit(): void {
    this.helper30057Service.formacionAcademicaForm
      .get('nivelesAcademicos')
      .valueChanges.subscribe(() => {
        this.arrayFormaciones = this.helper30057Service.formacionAcademicaForm.get(
          'nivelesAcademicos'
        ).value;
      });

    this.maestraRepository
      .getMaestraDetalleByCod('TBL_MAE_TIPO_CONO')
      .subscribe((items) => {
        this.maestraRepository
          .getMaestraConocimiento(
            items.find((item) => item.codProg === Const.COD_CON_TEC)
              .maeDetalleId
          )
          .subscribe((res) => {
            this.listaConocimientoTecnico = {
              listagrupado: res.conocimientoAgrupado,
              listoriginal: res.listaoriginal,
            };
          });
        this.maestraRepository
          .getMaestraConocimiento(
            items.find((item) => item.codProg === Const.COD_CUR_ESP)
              .maeDetalleId
          )
          .subscribe((res) => {
            this.listaCursosEsp = {
              listagrupado: res.conocimientoAgrupado,
              listoriginal: res.listaoriginal,
            };
          });
        this.maestraRepository
          .getMaestraConocimiento(
            items.find((item) => item.codProg === Const.COD_PRO_ESP)
              .maeDetalleId
          )
          .subscribe((res) => {
            this.listaProgramEsp = {
              listagrupado: res.conocimientoAgrupado,
              listoriginal: res.listaoriginal,
            };
          });
        this.maestraRepository
          .getMaestraConocimiento(
            items.find((item) => item.codProg === Const.COD_CON_OFI)
              .maeDetalleId
          )
          .subscribe((res) => {
            this.listaConOfi = {
              listagrupado: res.conocimientoAgrupado,
              listoriginal: res.listaoriginal,
            };
          });
        this.maestraRepository
          .getMaestraConocimiento(
            items.find((item) => item.codProg === Const.COD_CON_IDIO)
              .maeDetalleId
          )
          .subscribe((res) => {
            this.listaConIdio = {
              listagrupado: res.conocimientoAgrupado,
              listoriginal: res.listaoriginal,
            };
          });
      });
  }

  openModal(data?: any) {
    const modalNivelEducativo = this.dialog.open(ModalNivelEducatvoComponent, {
      data,
      width: '56rem',
    });
    modalNivelEducativo.afterClosed().subscribe((res) => {
      if (res) {
        this.helper30057Service.formacionAcademicaForm.markAsDirty();
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
            grado: res.grado,
            nivel: res.nivel,
            nombreGrado: res.nombreGrado,
            tipoGrado: res.tipoGrado,
            tipoNivel: res.tipoNivel,
            carreras: res.carreras,
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

  deleteItem(event) {
    const order = this.arrayFormaciones.indexOf (event);
    this.helper30057Service.formacionAcademicaForm
      .get('nivelesAcademicosToDelete')
      .patchValue([
        ...this.helper30057Service.formacionAcademicaForm.get(
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
}
