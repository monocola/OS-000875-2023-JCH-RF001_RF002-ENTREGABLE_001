export interface TableColumn2 {
  name: string;
  dataKey: string;
  position?: 'right' | 'left' | 'center';
  isSortable?: boolean;
  width?: string;
  descripcion?: string;
  flagSeleccion?: boolean;
  flagCheck?: boolean;
  flagCombo?: boolean;
}
