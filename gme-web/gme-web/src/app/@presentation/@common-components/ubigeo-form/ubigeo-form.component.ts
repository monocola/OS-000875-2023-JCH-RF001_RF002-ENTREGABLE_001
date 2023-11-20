import {
  Component,
  Input,
  OnChanges,
  OnInit,
  SimpleChanges,
} from '@angular/core';
import { FormControl } from '@angular/forms';
import { ParameterRepository } from 'src/app/@domain/repository/parameter.repository';
import { forkJoin } from 'rxjs';

@Component({
  selector: 'gme-web-ubigeo-form',
  templateUrl: './ubigeo-form.component.html',
  styleUrls: ['./ubigeo-form.component.scss'],
})
export class UbigeoFormComponent implements OnInit, OnChanges {
  @Input() departamentoControl: FormControl;
  @Input() provinciaControl: FormControl;
  @Input() distritoControl: FormControl;
  @Input() updateArray: number[] = [];

  departamentos = [];
  provincias = [];
  distritos = [];

  constructor(private parametrosRepository: ParameterRepository) {}

  ngOnChanges(changes: SimpleChanges): void {
    if (changes?.updateArray?.currentValue) {
      const ubigeoArray = changes.updateArray.currentValue;
      if (ubigeoArray.length > 0) {
        this.updateForm(ubigeoArray);
      }
    }
  }

  ngOnInit(): void {
    this.loadCombox();
  }

  loadCombox() {
    const getDepartamentos = this.parametrosRepository.getDepartamento();
    forkJoin([getDepartamentos]).subscribe((results) => {
      this.departamentos = results[0];
    });
  }

  cambioDept(idDept: number) {
    this.cleanProvincias();
    this.cleanDistritos();
    if (idDept) {
      this.parametrosRepository.getProvincias(idDept).subscribe((value) => {
        this.provincias = value;
      });
    }
  }

  cambioProv(idProv: number) {
    this.cleanDistritos();
    if (idProv) {
      this.parametrosRepository.getDistritos(idProv).subscribe((value) => {
        this.distritos = value;
      });
    }
  }

  cleanDistritos() {
    this.distritos = [];
    this.distritoControl.setValue('');
  }

  cleanProvincias() {
    this.provincias = [];
    this.distritos = [];
    this.provinciaControl.setValue('');
    this.distritoControl.setValue('');
  }

  updateForm(arrayValues) {
    const idDept = arrayValues[0];
    const idProv = arrayValues[1];
    const idDist = arrayValues[2];

    this.cleanProvincias();
    this.cleanDistritos();

    if (idDept) {
      this.parametrosRepository.getProvincias(idDept).subscribe((value) => {
        this.provincias = value;
        setTimeout(() => {
          this.departamentoControl.patchValue(idDept);
          this.provinciaControl.patchValue(idProv);
        }, 0);

        if (idProv) {
          this.parametrosRepository
            .getDistritos(idProv)
            .subscribe((distritos) => {
              this.distritos = distritos;
              setTimeout(() => {
                this.distritoControl.patchValue(idDist);
              }, 0);
            });
        }
      });
    }
  }
}
