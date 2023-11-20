import { Component, OnInit } from '@angular/core';
import { MatDialog } from '@angular/material/dialog';
import { Const } from 'src/app/@data/services/const';
import { BasesRepository } from 'src/app/@domain/repository/bases.repository';
import { CreacionBaseService } from '../creacion-base.service';
import { AuthenticationRepository } from '../../../../../@domain/repository/authentication.repository';
import { Observable } from 'rxjs';
import { Declaracionjurada } from '../../../../../@data/model/bases/declaracionjurada';
import { map, startWith } from 'rxjs/operators';
import { FormArray, FormBuilder, FormGroup, Validators } from '@angular/forms';
import { Utils } from '../../../../../utils/utils';

@Component({
  selector: 'serv-talento-step3',
  templateUrl: './step3.component.html',
  styleUrls: ['./step3.component.scss'],
})
export class Step3Component implements OnInit {
  declaracionesJuradas: any[] = [];
  declaracionesJuradasEntidad: any[] = [];
  declaracionesJuradasServir: any[] = [];
  filteredDeclaracionOptions: Observable<Declaracionjurada[]>;
  const = Const;
  constructor(
    public helperService: CreacionBaseService,
    private basesService: BasesRepository,
    private dialog: MatDialog,
    public authRepository: AuthenticationRepository,
    private formBuilder: FormBuilder
  ) {
    this.d.valueChanges.subscribe((item) => {});
  }

  ngOnInit(): void {}

  get d(): FormArray {
    return this.f.declaraJuradaRequeridos as FormArray;
  }

  get declaracionFormGroups() {
    return this.d.controls as FormGroup[];
  }

  get f() {
    return this.helperService.form3.controls;
  }

  addDeclaration(callGlobal: boolean) {
    if (this.declaracionFormGroups.length > 0) {
      let validdata = this.d.controls.filter(
        (item: FormGroup) =>
          item.controls.name.value != null &&
          item.controls.name.value.toString().trim().length !== 0
      );
      if (validdata.length !== this.declaracionFormGroups.length) {
        this.declaracionFormGroups[
          this.d.controls.length - 1
        ].controls.name.enable();
        return;
      }
    }
    if (callGlobal && this.d.controls.length > 0) {
      this.declaracionFormGroups[
        this.declaracionFormGroups.length - 1
      ].controls.name.disable();
    }
    this.d.push(
      this.formBuilder.group(
        {
          name: ['', Validators.required],
          datasend: [],
          filtros: [this.getClearList()],
          legacy: [false],
        },
        {
          validators: [Utils.notCorrectDataValidator],
        }
      )
    );
    this.filteredDeclaracionOptions = this.declaracionFormGroups[
      this.declaracionFormGroups.length - 1
    ].valueChanges.pipe(
      startWith(''),
      map((filterString: any) => {
        return this.filter(
          typeof filterString === 'string' ? filterString : filterString.name
        );
      })
    );
  }

  private filter(value: string): Declaracionjurada[] {
    let filtro: Declaracionjurada[] = [];
    if (value !== undefined) {
      const filterValue = value.toLowerCase();
      filtro = this.getClearList().filter((optionValue) => {
        return optionValue.descripcion.toLowerCase().includes(filterValue);
      });
    }
    return filtro;
  }

  private getClearList() {
    return this.helperService.declaraJurada.filter(
      (item) =>
        !this.declaracionFormGroups
          .map((group) => group.controls.name.value)
          .includes(item.descripcion)
    );
  }

  deleteDeclaration(indexFormGroup: number) {
    let ValdecJuradaToDelete = this.declaracionFormGroups[indexFormGroup].value
      .datasend;
    this.declaracionFormGroups.forEach((e) => {
      e.value.filtros.value = this.helperService.declaraJurada.filter(
        (item) =>
          item.descripcion !==
          this.declaracionFormGroups[indexFormGroup].controls.name.value
      );
    });

    if (this.d.value[indexFormGroup].legacy) {
      this.helperService.form3.controls.declaraJuradaRequeridosToDelete.value.push(
        ValdecJuradaToDelete
      );
    }

    this.d.removeAt(indexFormGroup);
    if (this.d.controls.length > 0) {
      let lastItemControl = this.declaracionFormGroups[
        this.d.controls.length - 1
      ];
      if (!lastItemControl.controls.legacy.value) {
        lastItemControl.controls.name.enable();
      }
    }
  }

  addNewAndCancelEdit(i: number) {
    this.autocompletData(i, this.declaracionFormGroups[i].controls.name.value);
    this.declaracionFormGroups[i].controls.name.disable();
    this.declaracionFormGroups.forEach((item) => {
      item.controls.filtros.setValue(this.helperService.declaraJurada);
    });
    this.addDeclaration(false);
  }

  autocompletData(indexFormGroup: number, result: string) {
    if (!this.declaracionFormGroups[indexFormGroup].value.legacy) {
      let dataSelect = this.helperService.declaraJurada.find(
        (item) => item.descripcion === result
      );
      if (dataSelect != null) {
        dataSelect.idBase = this.helperService.idBase;
        this.declaracionFormGroups[indexFormGroup].controls.datasend.setValue(
          dataSelect
        );
      }
    }
  }
}
