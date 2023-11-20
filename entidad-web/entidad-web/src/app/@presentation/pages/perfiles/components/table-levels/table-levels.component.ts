import {
  NbSortDirection,
  NbSortRequest,
  NbTreeGridDataSource,
  NbTreeGridDataSourceBuilder,
  NbTreeGridPresentationNode,
} from '@nebular/theme';
import {
  Component,
  EventEmitter,
  Input,
  OnChanges,
  OnInit,
  Output,
  SimpleChanges,
} from '@angular/core';
import { HelperLeyComponentsPerfilesService } from '../helperComponentPerfiles.service';
import { removeDuplicatedItems } from 'src/app/utils/converterFile';

@Component({
  selector: 'serv-talento-table-levels',
  templateUrl: './table-levels.component.html',
  styleUrls: ['./table-levels.component.scss'],
})
export class TableLevelsComponent implements OnChanges, OnInit {
  @Input() data = [];
  @Output() editEmitter = new EventEmitter();
  @Output() deleteEmitter = new EventEmitter();

  arrayBuilded = [];
  allRows = [];

  dataSource: NbTreeGridDataSource<any>;
  sortColumn: string;
  sortDirection: NbSortDirection = NbSortDirection.NONE;

  dataToRender: any[] = [];

  customColumn1 = 'NIVEL EDUCATIVO';
  defaultColumnsNames = ['GRADO', 'SITUACIÓN ACADÉMICA'];
  defaultColumns = ['grado', 'situacion'];
  customColumn2 = 'ACCIONES';
  customColumn3 = 'CARRERAS';

  allColumns = [
    this.customColumn1,
    ...this.defaultColumns,
    this.customColumn3,
    this.customColumn2,
  ];

  constructor(
    private dataSourceBuilder: NbTreeGridDataSourceBuilder<any>,
    private helperService: HelperLeyComponentsPerfilesService
  ) {}

  ngOnInit(): void {}

  ngOnChanges(changes: SimpleChanges): void {
    if (changes.data?.currentValue) {
      const aux = this.buildTree(this.data);
      this.arrayBuilded = this.transformJSON(aux);
      this.dataSource = this.dataSourceBuilder.create(this.arrayBuilded);
    }
  }

  buildTree(array: any[]) {
    const tree = [];
    const niveles = array.map((el) => el.nivel);
    const nivelesUnicos = removeDuplicatedItems(niveles);
    nivelesUnicos.map((nivel) => {
      tree.push({
        nivel,
        children: this.buildSubTree(array, nivel),
      });
    });
    return tree;
  }

  buildSubTree(array: any[], nivel) {
    const subniveles = array.filter((el) => el.nivel === nivel);
    const tipoNiveles = subniveles.map((el) => el.tipoNivel);
    const tipoNivelesUnicos = removeDuplicatedItems(tipoNiveles);
    return tipoNivelesUnicos.map((subnivel) => {
      return {
        subnivel,
        children: array.filter(
          (el) => el.tipoNivel === subnivel && el.nivel === nivel
        ),
      };
    });
  }

  getData(array, field, id) {
    return array.filter((el) => el[field] === id);
  }

  transformJSON(nivelesEducativos) {
    const aux = nivelesEducativos.map((n) => this.setPrimerNivel(n));
    return aux;
  }

  setPrimerNivel(nivel) {
    const data = this.getData(
      this.helperService.nivelesEducativos,
      'maeDetalleId',
      nivel.nivel
    )[0];

    return {
      data: {
        descripcion: data.descripcion,
        restOfData: data,
      },
      children: this.setSegundoNivel(nivel.children, data),
      expanded: true,
    };
  }

  setSegundoNivel(tiposNivel, nivel) {
    const aux = tiposNivel.map((tipoNivel) =>
      this.setSegundoNivelHelper(tipoNivel, nivel)
    );
    return aux;
  }

  setSegundoNivelHelper(tipoNivel, nivel) {
    const data = this.getData(
      this.helperService.estadosNiveles,
      'maeDetalleId',
      tipoNivel.subnivel
    )[0];

    return {
      data: {
        descripcion: data.descripcion,
        restOfData: { nivel, tipoNivel: data },
      },
      children: this.setTercerNivel(tipoNivel.children, nivel, data),
      expanded: true,
    };
  }

  setTercerNivel(grados, nivel, tipoNivel) {
    const aux = grados.map((grado) =>
      this.setTercerNivelHelper(grado, nivel, tipoNivel)
    );
    return aux;
  }

  setTercerNivelHelper(grado, nivel, tipoNivel) {
    const data = this.getData(
      this.helperService.grados,
      'maeDetalleId',
      grado.grado
    )[0];
    const dataGrado = this.getData(
      this.helperService.estadosGrado,
      'maeDetalleId',
      grado.tipoGrado
    )[0];
    return {
      data: {
        formacionAcademicaId: data.formacionAcademicaId,
        estado: data.estado,
        grado: data.descripcion,
        situacion: dataGrado?.descripcion || '-',
        carreras: grado.carreras,
        carrerasToDelete: grado.carrerasToDelete,
        carrerasToShow: grado.carreras.map((c) => ` ${c.descripcion}`),
        restOfData: { grado, nivel, tipoNivel },
      },
    };
  }

  updateSort(sortRequest: NbSortRequest): void {
    this.sortColumn = sortRequest.column;
    this.sortDirection = sortRequest.direction;
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

  editAction(row: NbTreeGridPresentationNode<any>) {
    this.editEmitter.emit(row.data);
  }

  deleteAction(row: NbTreeGridPresentationNode<any>) {
    this.deleteEmitter.emit(row.data);
  }

  getOnlyEvaluations(array, onlyEvaluations) {
    array.map((item) => {
      if (item.children) {
        this.getOnlyEvaluations(item.children, onlyEvaluations);
        if (item.data.restOfData.tipo) {
          onlyEvaluations.push(null);
        }
      } else {
        onlyEvaluations.push(item.data);
      }
    });
  }
}
