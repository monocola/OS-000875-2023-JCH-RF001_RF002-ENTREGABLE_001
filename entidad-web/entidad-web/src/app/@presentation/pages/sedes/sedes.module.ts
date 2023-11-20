import { MatAutocompleteModule } from '@angular/material/autocomplete';
import { NbIconModule } from '@nebular/theme';
import { NbInputModule } from '@nebular/theme';
import { CommonComponentsModule } from './../../@common-components/common-components.module';
import { NbFormFieldModule } from '@nebular/theme';
import { ReactiveFormsModule } from '@angular/forms';
import { FormsModule } from '@angular/forms';
import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { SedesRoutingModule } from './sedes-routing.module';
import { SedesComponent } from './sedes.component';
import { NbButtonModule, NbSelectModule } from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { ModalCreacionSedesComponent } from './modal-creacion-sedes/modal-creacion-sedes.component';


@NgModule({
  declarations: [
    SedesComponent,
    ModalCreacionSedesComponent
  ],
  imports: [
    CommonModule,
    SedesRoutingModule,
    NbButtonModule,
    MatDividerModule,
    NbSelectModule,
    FormsModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    CommonComponentsModule,
    NbInputModule,
    NbIconModule,
    MatAutocompleteModule
  ]
})
export class SedesModule { }
