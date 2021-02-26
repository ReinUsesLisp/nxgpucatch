#include <deko3d.hpp>

dk::UniqueDevice device;
dk::UniqueQueue queue;

void InitializeDevice() {
    device = dk::DeviceMaker{}
                 .setFlags(DkDeviceFlags_DepthMinusOneToOne | DkDeviceFlags_OriginUpperLeft)
                 .create();
    queue = dk::QueueMaker{device}
                .setPerWarpScratchMemorySize(128 * DK_PER_WARP_SCRATCH_MEM_ALIGNMENT)
                .create();
}