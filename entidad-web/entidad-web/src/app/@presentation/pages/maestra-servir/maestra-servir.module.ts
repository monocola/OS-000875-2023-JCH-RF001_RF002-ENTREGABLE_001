import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';

import { TablaMaestraRoutingModule } from './maestra-servir-routing.module';
import { TablaMaestraComponent } from './maestra-servir.component';
import {
  NbButtonModule,
  NbFormFieldModule,
  NbIconModule,
  NbInputModule,
  NbOptionModule,
  NbSelectModule,
} from '@nebular/theme';
import { MatDividerModule } from '@angular/material/divider';
import { ReactiveFormsModule } from '@angular/forms';
import { ModalCreacionDetalleMaestraComponent } from './modal-creacion-detalle-maestra/modal-creacion-detalle-maestra.component';
import { NgxTrimDirectiveModule } from 'ngx-trim-directive';
import { CommonComponentsModule } from '../../@common-components/common-components.module';

@NgModule({
  declarations: [TablaMaestraComponent, ModalCreacionDetalleMaestraComponent],
  imports: [
    CommonModule,
    TablaMaestraRoutingModule,
    NbButtonModule,
    MatDividerModule,
    NbFormFieldModule,
    NbOptionModule,
    NbSelectModule,
    CommonComponentsModule,
    ReactiveFormsModule,
    NbIconModule,
    NbInputModule,
    NgxTrimDirectiveModule,
  ],
})
export class TablaMaestraModule {}
