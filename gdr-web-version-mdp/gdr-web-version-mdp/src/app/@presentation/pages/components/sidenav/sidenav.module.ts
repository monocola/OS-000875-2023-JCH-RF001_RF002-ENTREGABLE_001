import { NgModule } from '@angular/core';
import { CommonModule } from '@angular/common';
import { ThemeModule } from '../../../@theme/theme.module';
import { NbButtonModule,
  NbFormFieldModule, NbCardModule,
  NbIconModule, NbInputModule,
  NbLayoutModule, NbSelectModule,
  NbToggleModule, NbSidebarModule
} from '@nebular/theme';
import { FormsModule, ReactiveFormsModule } from '@angular/forms';
import { MatIconModule } from '@angular/material/icon';
import { MatDividerModule } from '@angular/material/divider';
import { NbEvaIconsModule } from '@nebular/eva-icons';
@NgModule({

  imports: [
    CommonModule,
    ThemeModule,
    NbButtonModule,
    FormsModule,
    NbCardModule,
    NbSidebarModule,
    ReactiveFormsModule,
    NbIconModule,
    NbLayoutModule,
    MatDividerModule,
    NbSelectModule,
    NbInputModule,
    ReactiveFormsModule,
    NbFormFieldModule,
    NbEvaIconsModule,
    MatIconModule,
    NbInputModule,
    NbToggleModule
  ],
})
export class SidenavModule { }
