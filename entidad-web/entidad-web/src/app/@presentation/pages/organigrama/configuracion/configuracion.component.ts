import { Component, OnDestroy, OnInit } from '@angular/core';
import { FormControl, Validators } from '@angular/forms';
import { OrganigramaRepository } from 'src/app/@domain/repository/organigrama.repository';
import { TableColumn } from 'src/app/@presentation/@common-components/material-table/table-column';
@Component({
  selector: 'serv-talento-configuracion',
  templateUrl: './configuracion.component.html',
  styleUrls: ['./configuracion.component.scss']
})

export class ConfiguracionComponent implements OnInit, OnDestroy {

  control = new FormControl('1', Validators.required);
  createMode = false;
  editMode = false;
  ordersTableColumns: TableColumn[];

  editData?: any = null;

  constructor(
    private organigramaRepository: OrganigramaRepository
  ) { }

  ngOnInit(): void {
    this.handleOrganoStored();
  }

  ngOnDestroy(): void {
    this.organigramaRepository.setOrganoOrUnidadFromChart(null);
  }

  changeCombo() {
    this.createMode = false;
    this.editMode = false;
  }

  handleOrganoStored() {
    const item = this.organigramaRepository.getOrganoStored();
    if (item) {
      switch (item.desTipoOrgano) {
        case "TIPO ORGANO":
          this.control.setValue('1');
          break;
        case "TIPO UNIDAD ORGANICA":
          this.control.setValue('2');
          break;
      }
      this.createMode = true;
      this.editMode = true;
      this.editData = item;
    }
  }

  newStructure() {
    this.createMode = true;
    this.editMode = false;
  }

  handleClose(flag) {
    if (flag === 1) {
      this.createMode = false;
    } else {
      this.editMode = false;
    }
  }


}

