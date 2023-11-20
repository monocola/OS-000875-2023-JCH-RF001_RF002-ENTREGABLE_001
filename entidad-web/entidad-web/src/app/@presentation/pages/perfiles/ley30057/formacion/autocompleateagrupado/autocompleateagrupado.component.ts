import { Component, Input, OnInit } from '@angular/core';
import { of } from 'rxjs';
import { map, startWith } from 'rxjs/operators';
import { ConocimientoAgrupado, LstMaestraConocimiento } from '../../../../../../@data/model/maestra/conocimiento';
import { Observable } from 'rxjs/Observable';
import { AbstractControl, FormControl } from '@angular/forms';
import { Const } from 'src/app/@data/services/const';

@Component({
  selector: 'serv-talento-autocompleateagrupado',
  templateUrl: './autocompleateagrupado.component.html',
  styleUrls: ['./autocompleateagrupado.component.scss']
})
export class AutocompleateagrupadoComponent implements OnInit {
  @Input() placeholder: string = "";
  @Input() control: AbstractControl = null;
  @Input() listaConocimiento: Autocompleateagrupado = new Autocompleateagrupado();
  @Input() showHour: boolean = false;
  @Input() isIdioma: boolean = false;
  @Input() isOffice: boolean = false;
  @Input() isHourMore: boolean = false;
  @Input() maxlenghtisHM: number = 3;
  @Input() maxisHM: number = 100;

  inputValue = new FormControl();
  hourValue = new FormControl();
  currentSelectConocimiento: LstMaestraConocimiento = null;
  filteredGroups$: Observable<ConocimientoAgrupado[]>;

  COD_PEF_IDI_BAS: number = Const.COD_PEF_IDI_BAS;
  COD_PEF_IDI_INT: number = Const.COD_PEF_IDI_INT;
  COD_PEF_IDI_AVA: number = Const.COD_PEF_IDI_AVA;

  COD_PEF_OFI_BAS: number = Const.COD_PEF_OFI_BAS;
  COD_PEF_OFI_INT: number = Const.COD_PEF_OFI_INT;
  COD_PEF_OFI_AVA: number = Const.COD_PEF_OFI_AVA;

  constructor() { }

  ngOnInit(): void {
    this.inicializarAutocompleate();
  }

  private inicializarAutocompleate() {
    this.hourValue.setValue(0);
    this.filteredGroups$ = of(this.listaConocimiento.listagrupado);
    this.filteredGroups$ = this.inputValue.valueChanges
      .pipe(
        startWith(''),
        map((filterString: any) => {
          this.currentSelectConocimiento = null;
          return this.filter(filterString);
        }),
      );
  }

  private filterChildren(children: string[], filterValue: string) {
    return children.filter(optionValue => optionValue.toLowerCase().includes(filterValue));
  }

  private filter(value: string): ConocimientoAgrupado[] {
    const filterValue = value.toLowerCase();
    return this.listaConocimiento.listagrupado
      .map(group => {
        return {
          descripcionCategoria: group.descripcionCategoria,
          hijos: this.filterChildren(group.hijos, filterValue),
        };
      })
      .filter(group => group.hijos.length);
  }

  trackByFn(index, item) {
    return item.descripcionCategoria;
  }

  selectConocimientoChange(data: any) {
    console.info(data);
    this.currentSelectConocimiento = this.listaConocimiento.listoriginal.find(item => item.descripcion === data);
  }

  selectedConocimiento() {
    this.currentSelectConocimiento.horas = Number(this.hourValue.value);
    if (this.isIdioma) {
      this.currentSelectConocimiento.nivelDominioId = Number(this.hourValue.value);
    }
    if (this.isOffice) {
      this.currentSelectConocimiento.nivelDominioId = Number(this.hourValue.value);
    }
    this.control.value.push(this.currentSelectConocimiento);
    this.currentSelectConocimiento = null;
    this.inputValue.setValue("");
    this.hourValue.setValue(0);
  }

  deleteItemConocimiento(i: number, item: any) {
    console.info(item);
    item.estado = false;
  }

  setMaxValue() {
    this.hourValue.markAsDirty();
    if (this.isHourMore) {
      if (Number(this.hourValue.value) > 1200) {
        this.hourValue.patchValue('1200');
      }
    } else {
      if (Number(this.hourValue.value) > 100) {
        this.hourValue.patchValue('100');
      }
    }
  }

  disabledDoneButtom() {
    if (this.showHour) {
      return this.currentSelectConocimiento == null || Number(this.hourValue.value) === 0;
    } else {
      return this.currentSelectConocimiento == null;
    }
  }
}
export class Autocompleateagrupado {
  listagrupado: any[] = [];
  listoriginal: any[] = [];
}
